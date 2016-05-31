module UploadResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

open Utils
open Siren

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext
type ResponseInfo =
      | Success of SirenDocument
      | Failure

type Message = WebMessage of RequestInfo * AsyncReplyChannel<WebPart> | UploadCompleteNotification

