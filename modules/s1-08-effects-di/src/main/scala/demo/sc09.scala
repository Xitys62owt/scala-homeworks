package sc09

import scala.util.NotGiven


// Давайте отправлять HTTP запросы


enum Method:
    case GET, PUT, POST, DELETE

enum Proto:
    case Http, Https

enum Auth:
    case Unauth, Basic, Bearer

case class Request(method: Method, proto: Proto, host: String, port: Int, body: Option[String])

object ExternalService:
    def handle(req: Request): Either[String, String] =
        req match
            case Request(method, proto, host, port, body) if host.isEmpty() => Left("400: No HOST")
            case Request(method, proto, host, port, body) if port <= 0      => Left("400: No PORT")
            case Request(method, proto, host, port, body) if port >= 65535  => Left("400: Bad PORT")
            case Request(Method.GET, proto, host, port, Some(_))            => Left("400: Don't use Body with GET")
            case Request(Method.PUT, proto, host, port, None)               => Left("400: Need Body on PUT")
            case Request(Method.POST, proto, host, port, None)              => Left("400: Need Body on POST")
            case Request(Method.DELETE, proto, host, port, Some(_))         => Left("400: Don't use Body with DELETE")
            case _ => Right("200: OK")


object Uncontrolled extends App:

    class RequestBuilder(request: Request = Request(Method.GET, Proto.Http, "", 0, None)):
        def method(method: Method): RequestBuilder = RequestBuilder(request.copy(method = method))
        def proto(proto: Proto): RequestBuilder    = RequestBuilder(request.copy(proto = proto))
        def host(hostname: String): RequestBuilder = RequestBuilder(request.copy(host = hostname))
        def port(portnumber: Int): RequestBuilder  = RequestBuilder(request.copy(port = portnumber))
        def body(body: String): RequestBuilder     = RequestBuilder(request.copy(body = Some(body)))
        val build: Request = request
        
    def sendRequest(req: Request): String = ExternalService.handle(req) match
        case Left(value) => throw new Exception(s"Bad call > $value")
        case Right(value) => value
  

    val preRq = RequestBuilder().host("localhost").port(8080).method(Method.GET).proto(Proto.Http)

    val rq1 = preRq.build
    val rq2 = preRq.method(Method.POST).body("dhskjfhkj").build

    println(sendRequest(rq1))
    println(sendRequest(rq2))






object Controlled extends App:

    sealed trait Uninited
    sealed trait Inited

    class RequestBuilder[M, Pr, H, P, B](request: Request = Request(Method.GET, Proto.Http, "", 0, None)):

        def get(using =:=[M, Uninited], B =:= Uninited)   : RequestBuilder[Inited, Pr, H, P, Inited] =
            new RequestBuilder(request.copy(method = Method.GET))
        def post(using M =:= Uninited)                       : RequestBuilder[Inited, Pr, H, P, B] =
            new RequestBuilder(request.copy(method = Method.POST))
        def put(using M =:= Uninited)                       : RequestBuilder[Inited, Pr, H, P, B]  =
            new RequestBuilder(request.copy(method = Method.PUT))
        def delete(using M =:= Uninited, B =:= Uninited): RequestBuilder[Inited, Pr, H, P, Inited] =
            new RequestBuilder(request.copy(method = Method.DELETE))

        def body(body: String)(using B =:= Uninited): RequestBuilder[M, Pr, H, P, Inited] =
            new RequestBuilder(request.copy(body = Some(body)))

        def proto(proto: Proto):    RequestBuilder[M, Inited, H, P, B]  = new RequestBuilder(request.copy(proto = proto))
        def host(hostname: String): RequestBuilder[M, Pr, Inited, P, B] = new RequestBuilder(request.copy(host = hostname))
        def port(portnumber: Int):  RequestBuilder[M, Pr, H, Inited, B] = new RequestBuilder(request.copy(port = portnumber))

        def build(using M =:= Inited, Pr =:= Inited, H =:= Inited, P =:= Inited, B =:= Inited) : Request =
            request


    object RequestBuilder:
        def apply(): RequestBuilder[Uninited, Uninited, Uninited, Uninited, Uninited] =
            new RequestBuilder

    def sendRequest(req: Request) =
        ExternalService.handle(req) match
            case Left(value) => throw new Exception(s"Bad call > $value")
            case Right(value) => value


    val result = sendRequest(
        RequestBuilder()
            .proto(Proto.Http)
            .host("localhost")
            .port(8080)
            // .post
            .get
            // .body("DFSDFDSF")
            .build
    )

    println(result)





object TypeControlledSet extends App:

    sealed trait ReqParam
    case object Proto  extends ReqParam
    case object Host   extends ReqParam
    case object Port   extends ReqParam
    case object Method extends ReqParam
    case object Body   extends ReqParam
    case object Cookie extends ReqParam

    trait Request:
        def content: Set[ReqParam]

    object ExternalService:
        def handle(request: Request) =
            if (request.content(Method) || request.content.size > 1)
                "Request result"
            else
                throw new IllegalStateException


    sealed trait CompletedRequest

    class RequestTyped[T, C](val content: Set[ReqParam]) extends Request:
        def addParameter[F <: ReqParam](f: F)(using NotGiven[F <:< T]): RequestTyped[T & F, CompletedRequest] =
            new RequestTyped[T & F, CompletedRequest](content + f)


    object RequestTyped:
        def apply(f: Method.type): RequestTyped[Method.type, CompletedRequest] =
            new RequestTyped(Set(f))
        def apply[F <: ReqParam](f: F): RequestTyped[F, Nothing] =
            new RequestTyped(Set(f))


    val request1 = RequestTyped(Proto)
    // val request5 = request1.addParameter(Proto)
    val request2 = RequestTyped(Method)
    // val request6 = request2.addParameter(Method)
    val request3 = request1.addParameter(Method)
    val request4 = request1.addParameter(Body)
    val request7 = RequestTyped(Host).addParameter(Port)
    val request8 = RequestTyped(Host).addParameter(Port).addParameter(Method)

    
    def sendCompletedRequestC(request: RequestTyped[?, CompletedRequest]) =
        ExternalService.handle(request)
    def sendCompletedRequestT[T](request: RequestTyped[T, ?])(using T <:< Method.type) =
        ExternalService.handle(request)
    def sendRequestHostPortA[T, A <: T](request: RequestTyped[A & Host.type & Port.type, ?]) =
        ExternalService.handle(request)
    def sendRequestHostPortS(request: RequestTyped[Host.type & Port.type, ?]) =
        ExternalService.handle(request)


    // sendCompletedRequestC(request1)
    // sendCompletedRequestT(request1)
    sendCompletedRequestC(request2)
    sendCompletedRequestT(request2)
    sendCompletedRequestC(request3)
    sendCompletedRequestT(request3)
    sendCompletedRequestC(request4)
    // sendCompletedRequestT(request4)
    sendRequestHostPortA(request7)
    sendRequestHostPortA(request8)
    sendRequestHostPortS(request7)
    // sendRequestHostPortS(request8)


