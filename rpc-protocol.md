# TCP wire-protocol

Protocol buffers has a few limitations w.r.t. to TCP. Protobufs are not
delimited or self-describing. However, with an open TCP connection we need to
know how many bytes we expect to receive to make sure to receive the full
message. The [documentation recommends][protobufs-streaming] to implement your
own framing. The simplest approach, is to prefix every protobufs message by its
length in a fixed width format. We choose a `fixed64`, i.e. 64-bit unsigned
integer.

Furthermore, the [documentation says][protobufs-large] that protobufs are not
designed for large messages (more than 1 MiB). Instead, one should separate
messages into smaller chunks. For us that means that inlining block-data into
lambda programs (which will be encoded using protobufs) should be avoided.

For a TCP connection with the Mirage server we define the following protocol.

[protobufs-streaming]: https://developers.google.com/protocol-buffers/docs/techniques#streaming
[protobufs-large]: https://developers.google.com/protocol-buffers/docs/techniques#large-data


## Messages

Each message has a fixed size header defining the length of the contained
protobufs message, and the length and number of additional following blocks of
raw data. Once a TCP connection has been established the client can send a
`Request` and the server is expected to send a `Reply` in return. This repeats
until the connection is closed by either side. The binary encoding of messages
is as follows. See [`proto/request.proto`](proto/request.proto) for the
encoding of `Request` and `Reply`.

- `header`

    ```
    +----------------+----------------+---------------+
    | proto-size <8> | block-size <8> | block-num <8> |
    +----------------+----------------+---------------+
    ```

    - `proto-size`
        The size in bytes of the protobufs encoded message as `fixed64`.
    - `block-size`
        The size in bytes of one block as `fixed64`.
    - `block-num`
        The number of blocks following the protobufs encoded message as
        `fixed64`.

- `request`

    ```
    +-------------+----------------------+---------------------------------+
    | header <24> | Request <proto-size> | blocks <block-size x block-num> |
    +-------------+----------------------+---------------------------------+
    ```

    A request from the client to the server. For instance a request to execute
    a lambda program with some input blocks and return the result and output
    blocks. The request message may define the number of expected output
    blocks.

    If `n` is the number of input blocks and `m` the number of expected output
    blocks, then the sent lambda program should expect `n + m` string
    arguments. The first `n` will be the attached input blocks, the following
    `m` will be buffers to write the output blocks into.

    E.g. with `n = 3` and `m = 2`, the lambda program should have the type

    ```
    string -> string -> string    (* input blocks *)
    -> string -> string           (* output blocks *)
    -> a                          (* result *)
    ```

- `reply`

    ```
    +-------------+--------------------+---------------------------------+
    | header <24> | Reply <proto-size> | blocks <block-size x block-num> |
    +-------------+--------------------+---------------------------------+
    ```

    Response to request with success or failure. On success, the message will
    contain the result value and the expected output blocks will be appended to
    the message.


## Protocol

The protocol is very simple. The client opens a TCP connection to the server.
Once the connection is established, the client can send a `request` message,
and will expect a `reply` message in return. The sequence of `request`, `reply`
messages is repeated until the connection is closed by either side.

```
 Client                 Server

  <open>

    |       request        |
    | -------------------> |
    |                      |
    |        reply         |
    | <------------------- |
    |                      |
    |          .           |
    |          .           |
    |          .           |
    |                      |
    |       request        |
    | -------------------> |
    |                      |
    |        reply         |
    | <------------------- |

            <close>
```

Communication on a TCP connection is fully sequential. Parallel requests can
only be made through separate TCP connections. The client can keep a connection
pool to manage parallel requests.
