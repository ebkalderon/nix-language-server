use std::io::{self, Write};
use std::str;
use std::sync::Arc;

use bytes::{BufMut, BytesMut};
use futures::sync::mpsc;
use futures::{future, Sink};
use jsonrpc_core::IoHandler;
use log::{error, trace};
use nom::{Err, Needed};
use tokio;
use tokio::codec::{Decoder, Encoder, FramedRead, FramedWrite};
use tokio::prelude::{Future, Stream};

use crate::parser::parse_request;
use crate::Error;

/// Stdio server builder
#[derive(Debug)]
pub struct ServerBuilder {
    handler: Arc<IoHandler>,
}

impl ServerBuilder {
    /// Returns a new server instance
    pub fn new<T>(handler: T) -> Self
    where
        T: Into<IoHandler>,
    {
        ServerBuilder {
            handler: Arc::new(handler.into()),
        }
    }

    /// Will block until EOF is read or until an error occurs.
    /// The server reads from STDIN line-by-line, one request is taken
    /// per line and each response is written to STDOUT on a new line.
    pub fn build(&self) {
        let (sender, receiver) = mpsc::channel(1);

        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let framed_stdin = FramedRead::new(stdin, LanguageServerCodec::default());
        let framed_stdout = FramedWrite::new(stdout, LanguageServerCodec::default());

        let handler = self.handler.clone();
        let future = future::lazy(move || {
            let printer = receiver
                .map_err(|_| error!("failed to log message"))
                .forward(framed_stdout.sink_map_err(|e| error!("failed to encode response: {}", e)))
                .map(|_| ());

            tokio::spawn(printer);

            framed_stdin
                .map_err(|e| error!("failed to decode request: {}", e))
                .for_each(move |line| {
                    let sender = sender.clone();
                    process(&handler, line).and_then(move |resp| {
                        sender.send(resp).map(|_| ()).map_err(|_| unreachable!())
                    })
                })
        });

        tokio::run(future);
    }
}

/// Process a request asynchronously
fn process(io: &Arc<IoHandler>, input: String) -> impl Future<Item = String, Error = ()> + Send {
    trace!("received request: {}", input);
    io.handle_request(&input).map(move |result| {
        if let Some(res) = result {
            trace!("sending response: {}", res);
            res
        } else {
            trace!("request produced no response: {}", input);
            String::new()
        }
    })
}

#[derive(Debug, Default)]
struct LanguageServerCodec {
    remaining_msg_bytes: usize,
}

impl Encoder for LanguageServerCodec {
    type Item = String;
    type Error = Error;

    fn encode(&mut self, item: Self::Item, dst: &mut BytesMut) -> Result<(), Self::Error> {
        if !item.is_empty() {
            dst.reserve(item.len() + 60);
            let mut writer = dst.writer();
            write!(writer, "Content-Length: {}\r\n\r\n{}", item.len(), item)?;
            writer.flush()?;
        }

        Ok(())
    }
}

impl Decoder for LanguageServerCodec {
    type Item = String;
    type Error = Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if self.remaining_msg_bytes > src.len() {
            return Ok(None);
        }

        let string = str::from_utf8(src)?;
        let (message, len) = match parse_request(string) {
            Ok((remaining, message)) => (message.to_string(), src.len() - remaining.len()),
            Err(Err::Incomplete(Needed::Size(min))) => {
                self.remaining_msg_bytes = min;
                return Ok(None);
            }
            Err(Err::Incomplete(_)) => {
                return Ok(None);
            }
            Err(err) => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("parsing '{}' resulted in error: {:?}", string, err),
                ))?
            }
        };

        src.advance(len);
        self.remaining_msg_bytes = 0;

        Ok(Some(message))
    }
}
