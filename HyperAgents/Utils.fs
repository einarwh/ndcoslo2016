module Utils

type Agent<'T> = MailboxProcessor<'T>

let linkTo relativeUrl = "http://localhost:8083/" + relativeUrl


