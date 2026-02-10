# es-proxy

A transparent Elasticsearch proxy that logs all requests and responses with UUID correlation for auditing. Written in Gerbil Scheme.

## Overview

es-proxy sits between Elasticsearch clients and coordinator nodes, logging comprehensive request/response metadata to enable forensic analysis and auditing with minimal performance overhead.

## Installation

```bash
make build
make install
```

## Usage

```bash
es-proxy <remote-host> <remote-port> <local-host> <local-port> [--no-tls] [--cert PATH] [--key PATH]
```

Example:
```bash
es-proxy es-prod-001.example.net 9200 0.0.0.0 9200
```

With plain HTTP (no TLS):
```bash
es-proxy localhost 9200 0.0.0.0 9200 --no-tls
```

The process runs under systemd as `es-proxy.service`.

## Log Format

Each request/response pair is correlated by UUID:

**Request:**
```
TIMESTAMP ID: UUID User: USERNAME Path: /path Query: PARAMS Method: GET Source: IP Agent: USER_AGENT Body: BASE64
```

**Response:**
```
TIMESTAMP ID: UUID User: USERNAME Path: /path Query: PARAMS Method: GET Source: IP Agent: USER_AGENT Status: 200 Duration: 0.005s Size: 1234
```

## Testing

Run all tests:
```bash
make test
```

Run specific test suites:
```bash
make test-functional   # Functional tests (mock ES)
make test-memory       # Memory leak detection test
make test-stress       # Extended stress test (5 minutes)
```

The test suite includes:
- **MockElasticsearch** - A mock ES server handling typical endpoints (cluster info, health, search, bulk indexing, cat APIs)
- **Response validation** - Validates that mock responses match request parameters (search size/from, bulk item counts, document IDs)
- **Error handling** - Tests proper error responses for invalid JSON input
- **Proxy integration** - Tests full request/response proxying through es-proxy in plain HTTP mode
- **High-volume concurrency** - Validates behavior under 100 concurrent requests from 10 clients
- **Memory leak detection** - Tracks heap growth across iterations to detect memory leaks
- **Stress testing** - 5-minute sustained load with 50 concurrent clients, varying request sizes, and memory monitoring

### Response Validation

The mock Elasticsearch server parses request bodies and generates contextual responses:
- **Search**: Returns hits matching requested `size` and `from` parameters
- **Bulk**: Returns one item per document, preserving `_index` and `_id` from request
- **Index document**: Returns correct index name and generates/preserves document ID
- **Invalid JSON**: Returns 400 status with `parse_exception` error

## Makefile Targets

Run `make help` for a full list of available targets:

```
make build        # Build the es-proxy binary
make test         # Run all tests
make test-stress  # Run extended stress test (5 minutes)
make install      # Install binary to /usr/local/bin
make deps         # Install package dependencies
make clean        # Remove build artifacts
make help         # Show all targets
```

## Development

Build from source:
```bash
make build
```

Run tests:
```bash
make test
```
# gerbil-es-proxy
