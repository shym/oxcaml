# Multi-architecture Dockerfile for OxCaml Compiler
# Supports both ARM64 (aarch64) and x86_64 architectures
#
# BASIC USAGE:
#   Build:  docker build -t oxcaml .
#   Run:    docker run -it --rm oxcaml
#
# BUILD TARGETS:
#   oxcaml       - Minimal compiler image (default)
#   oxcaml-dev   - Development image with LLVM/LLDB tools
#   oxcaml-full  - Complete build environment
#
# CONFIGURATION EXAMPLES:
#   Basic (Flambda2):
#     docker build -t oxcaml .
#
#   With Runtime5:
#     docker build --build-arg CONFIG_OPTS="--enable-middle-end=flambda2 --enable-runtime5" -t oxcaml:runtime5 .
#
#   With Frame Pointers:
#     docker build --build-arg CONFIG_OPTS="--enable-middle-end=flambda2 --enable-frame-pointers" -t oxcaml:fp .
#
#   Development (LLDB for debugging):
#     docker build --target oxcaml-dev -t oxcaml:dev .
#
#   Development (llvmize tools):
#     docker build --target oxcaml-dev \
#       --build-arg CONFIG_OPTS="--enable-middle-end=flambda2 --enable-runtime5 --enable-frame-pointers" \
#       --build-arg OXCAML_LLVM_TAG=oxcaml-llvmize-16.0.6-minus3 \
#       --build-arg OXCAML_LLVM_SOURCE_BRANCH=llvmize-oxcaml \
#       -t oxcaml:llvmize .
#
#   O3 Optimizations:
#     docker build --build-arg OCAMLPARAM="_,O3=1" -t oxcaml:o3 .
#
#   Address Sanitizer:
#     docker build --build-arg CONFIG_OPTS="--enable-middle-end=flambda2 --enable-address-sanitizer" \
#       --build-arg CC=clang -t oxcaml:asan .
#
#   Multi-domain Support:
#     docker build --build-arg CONFIG_OPTS="--enable-middle-end=flambda2 --enable-runtime5 --enable-stack-checks --enable-poll-insertion --enable-multidomain" \
#       -t oxcaml:multidomain .
#
# MULTI-ARCHITECTURE:
#   Build for multiple architectures:
#     docker buildx build --platform linux/amd64,linux/arm64 -t oxcaml .
#
#   Build for specific architecture:
#     docker buildx build --platform linux/arm64 -t oxcaml:arm64 .
#     docker buildx build --platform linux/amd64 -t oxcaml:amd64 .
#
# RUNNING:
#   Interactive shell:
#     docker run -it --rm oxcaml
#
#   Compile a file (mount current directory):
#     docker run -it --rm -v $(pwd):/workspace oxcaml oxcamlopt myfile.ml
#
#   Development with source mounted:
#     docker run -it --rm -v $(pwd):/workspace/oxcaml oxcaml:dev
#
#   Run tests:
#     docker run -it --rm oxcaml:dev bash -c "cd /workspace/oxcaml && make test"

FROM ubuntu:24.04 AS base

# Install system dependencies
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    build-essential \
    autoconf \
    wget \
    curl \
    git \
    patch \
    parallel \
    afl++ \
    clang \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

# Stage 1: Build OCaml 4.14 with dune and menhir
FROM base AS ocaml-414

# Clone and build OCaml 4.14
RUN git clone --depth 1 --branch 4.14 https://github.com/ocaml/ocaml.git ocaml-414

WORKDIR /workspace/ocaml-414

# Copy the ARM64 patch for OCaml 4.14
COPY arm64-issue-debug-upstream.patch /workspace/

# Configure, patch, build and install OCaml 4.14
RUN ./configure --prefix=/opt/ocaml-414 && \
    patch -p1 < /workspace/arm64-issue-debug-upstream.patch && \
    make -j$(nproc) world.opt && \
    make install && \
    # Clean up to reduce layer size
    cd /workspace && rm -rf ocaml-414

ENV PATH="/opt/ocaml-414/bin:${PATH}"

# Build dune 3.15.3
WORKDIR /workspace
RUN git clone --depth 1 --branch 3.15.3 https://github.com/ocaml/dune.git dune

WORKDIR /workspace/dune
RUN make release && \
    cp _boot/dune.exe /opt/ocaml-414/bin/dune && \
    cd /workspace && rm -rf dune

# Build menhir 20231231
WORKDIR /workspace
RUN git clone --depth 1 --branch 20231231 https://github.com/LexiFi/menhir.git menhir

WORKDIR /workspace/menhir
RUN dune build && \
    cp _build/install/default/bin/menhir /opt/ocaml-414/bin/menhir && \
    # Copy menhirLib files
    mkdir -p /opt/ocaml-414/lib/menhirLib && \
    cat _build/install/default/lib/menhirLib/menhirLib.mli > /opt/ocaml-414/lib/menhirLib/menhirLib.mli && \
    cat _build/install/default/lib/menhirLib/menhirLib.ml > /opt/ocaml-414/lib/menhirLib/menhirLib.ml && \
    cd /workspace && rm -rf menhir

# Stage 2: Build OxCaml compiler
FROM ocaml-414 AS oxcaml-builder

ARG CONFIG_OPTS="--enable-middle-end=flambda2"
ARG BUILD_OCAMLPARAM=""
ARG OCAMLPARAM=""
ARG JOBS=4
ARG CC=gcc

WORKDIR /workspace/oxcaml

# Copy OxCaml source code
COPY . .

# Configure and build OxCaml
RUN autoconf && \
    CC=${CC} ./configure \
    --prefix=/opt/oxcaml \
    --with-dune=/opt/ocaml-414/bin/dune \
    ${CONFIG_OPTS} && \
    BUILD_OCAMLPARAM="${BUILD_OCAMLPARAM}" \
    OCAMLPARAM="${OCAMLPARAM}" \
    make -j${JOBS} compiler && \
    make install

# Stage 3: Minimal OxCaml runtime image
FROM base AS oxcaml

# Copy only the installed OxCaml compiler
COPY --from=oxcaml-builder /opt/oxcaml /opt/oxcaml

ENV PATH="/opt/oxcaml/bin:${PATH}"

WORKDIR /workspace

CMD ["/bin/bash"]

# Stage 4: Development image with LLVM tools (for DWARF and llvmize tests)
FROM oxcaml AS oxcaml-dev

ARG OXCAML_LLVM_TAG=oxcaml-lldb-16.0.6-minus1
ARG OXCAML_LLVM_SOURCE_BRANCH=lldb
ARG TARGETARCH

# Install custom OxCaml LLVM/LLDB
RUN if [ "${TARGETARCH}" = "amd64" ]; then \
        ARCH="x86_64"; \
    elif [ "${TARGETARCH}" = "arm64" ]; then \
        ARCH="aarch64"; \
    else \
        echo "Unsupported architecture: ${TARGETARCH}"; \
        exit 1; \
    fi && \
    OXCAML_LLVM_URL="https://github.com/ocaml-flambda/llvm-project/releases/download/${OXCAML_LLVM_TAG}/${OXCAML_LLVM_TAG}-llvm-linux-${ARCH}-${OXCAML_LLVM_SOURCE_BRANCH}.tar.gz" && \
    wget -O /tmp/oxcaml-llvm.tar.gz "${OXCAML_LLVM_URL}" && \
    mkdir -p /opt/oxcaml-llvm && \
    tar -xzf /tmp/oxcaml-llvm.tar.gz -C /opt/oxcaml-llvm --strip-components=1 && \
    rm /tmp/oxcaml-llvm.tar.gz

ENV PATH="/opt/oxcaml-llvm/bin:${PATH}"
ENV OXCAML_LLDB="/opt/oxcaml-llvm/bin/lldb"
ENV OXCAML_CLANG="/opt/oxcaml-llvm/bin/clang"

# Copy OCaml 4.14 for running tests
COPY --from=ocaml-414 /opt/ocaml-414 /opt/ocaml-414

# Ensure both OCaml 4.14 and OxCaml are in PATH (OxCaml takes precedence)
ENV PATH="/opt/oxcaml/bin:/opt/ocaml-414/bin:/opt/oxcaml-llvm/bin:${PATH}"

# Copy the full OxCaml source tree for testing
COPY --from=oxcaml-builder /workspace/oxcaml /workspace/oxcaml

WORKDIR /workspace/oxcaml

CMD ["/bin/bash"]

# Stage 5: Full build environment (includes build tree)
FROM oxcaml-dev AS oxcaml-full

# This stage contains everything: source, build artifacts, and tools
# Useful for development and debugging

WORKDIR /workspace/oxcaml

CMD ["/bin/bash"]
