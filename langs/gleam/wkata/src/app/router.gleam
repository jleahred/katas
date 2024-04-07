import wisp.{type Request, type Response}
import gleam/string_builder
import app/web
import gleam/io
import gleam/dynamic

/// The HTTP request handler- your application!
/// 
pub fn handle_request(req: Request) -> Response {
  io.debug(dynamic.from(req))
  io.debug(dynamic.classify(dynamic.from(req)))
  io.debug(req)

  // Apply the middleware stack for this request/response.
  use _req <- web.middleware(req)

  // Later we'll use templates, but for now a string will do.
  let body = string_builder.from_string("<h1>Hello, Jose!11134567</h1>")

  // Return a 200 OK response with the body and a HTML content type.
  wisp.html_response(body, 200)
}
