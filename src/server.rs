use std::collections::HashMap;
use std::error::Error;
use std::io::{BufReader, Read};
use std::io::prelude::*;
use std::net::{TcpListener, TcpStream};

fn send_response(mut stream: TcpStream) -> Result<(), Box<dyn Error>> {
    let response = format!("HTTP/1.1 200 OK\r\nContent-Type: text/json\r\n\r\n");

    stream.write(response.as_bytes())?;
    stream.flush()?;

    Ok({})
}

fn handle_connection(stream: TcpStream) -> Result<(), Box<dyn Error>> {
    let mut reader = BufReader::new(stream);
    let reader_ref = reader.by_ref();

    let mut request = String::new();
    reader_ref.read_line(&mut request)?;
    println!("Request: {}", request);

    let _headers: HashMap<String, String> = reader_ref.lines()
        .take_while(|line| line.as_ref().unwrap() != "")
        .map(|line| {
            let line = line.unwrap();
            // FIXME: Replace split with regex. Preferably a regex translated from spec.
            let split_line: Vec<&str> = line.split(":").collect();
            let header_name: &str = split_line.get(0).unwrap_or(&"unknown");
            let header_value: &str = split_line.get(1).unwrap_or(&"unknown");
            (String::from(header_name), String::from(header_value))
        }).collect();

    send_response(reader.into_inner())?;

    Ok({})
}

fn main() {
    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();
    for stream in listener.incoming() {
        let stream = stream.unwrap();

        handle_connection(stream).unwrap();
    }
}
