package sc11

import zio.*

//

type X = String

opaque type InnerId <: Int = Int
object InnerId:
    def from(id: Int): InnerId = id

opaque type OuterId <: Int = Int
object OuterId:
    def from(id: Int): OuterId = id


// Service pattern:

trait UserRepo:
    def find(userName: String): Task[InnerId]
    def take(userId: InnerId): Task[String]

trait UserId:
    def getInner(userId: OuterId): Task[InnerId]
    def getOuter(userId: InnerId): Task[OuterId]

trait Esia:
    def getInn(userId: OuterId): Task[String]
    def getAge(userId: OuterId): Task[Int]

trait SignOn:
    def authenticate(userName: String, inn: String): Task[InnerId]

trait Messager:
    def send(sender: InnerId, correspondent: String, message: String): Task[Unit]

// 

object UserRepo:
    def apply(ref: Ref[Map[Int, String]]): UserRepo = new:
        override def find(userName: String): Task[InnerId] =
            ZIO
                .succeed(userName.length()*7)
                .tap(id => ref.update(_ + (id -> userName)))
                .map(InnerId.from)
        override def take(userId: InnerId): Task[String] =
            ref.get.flatMap(cache => ZIO.attempt(cache(userId)))

    val layer: ZLayer[Any, Nothing, UserRepo] =
        ZLayer.fromZIO(
            Ref.make(Map.empty[Int, String])
                .map(ref => UserRepo(ref))
        )


object UserId:
    def apply(): UserId = new:
        override def getInner(userId: OuterId): Task[InnerId] =
            ZIO.succeed(InnerId.from(userId*7))
        override def getOuter(userId: InnerId): Task[OuterId] =
            ZIO.succeed(OuterId.from(userId/7))

    val layer: ULayer[UserId] =
        ZLayer.succeed(UserId())


object Esia:
    class Implementation extends Esia:
        override def getInn(userId: OuterId): Task[String] =
            ZIO.succeed(((userId*13)%11).toHexString)
        override def getAge(userId: OuterId): Task[Int] =
            ZIO.succeed(userId%7 + 16)

    val layer: ULayer[Esia] =
        ZLayer.succeed(new Implementation)


object SignOn:
    private val UnauthError = new Exception("Bad credintials")

    class Implementation(users: UserRepo, conv: UserId, esia: Esia) extends SignOn:
        override def authenticate(userName: String, inn: String): Task[InnerId] =
            for
                innerId <- users.find(userName)
                outerId <- conv.getOuter(innerId)
                userInn <- esia.getInn(outerId)
                _ <- ZIO.whenDiscard(userInn != inn)(ZIO.fail(UnauthError))
            yield innerId

    private def create(users: UserRepo, conv: UserId, esia: Esia): SignOn =
        new Implementation(users, conv, esia)

    val layer: ZLayer[UserRepo & UserId & Esia, Nothing, SignOn] =
        ZLayer.fromFunction(create)


object Messager:

    def apply(users: UserRepo): Messager = new:
        override def send(from: InnerId, correspondent: String, message: String): Task[Unit] =
            for
                sender <- users.take(from)
                _ <- Console.printLine(
                        s"""|New message:
                            |from: $sender
                            |to: $correspondent
                            |message: $message
                            |""".stripMargin
                    )
            yield ()

    private val instantiation: ZIO[UserRepo, Nothing, Messager] =
        ZIO.service[UserRepo].map(Messager.apply)

    val layer: ZLayer[UserRepo, Nothing, Messager] = 
        ZLayer.fromZIO(instantiation)


object ZioLayerDemo extends ZIOAppDefault:

    def program(from: String, to: String, message: String): ZIO[SignOn & Messager, Throwable, Unit] =
        for 
            // _ <- Console.printLine(((from.length()*13)%11).toHexString)
            userInput  <- Console.readLine(s"Input INN for $from: ")
            authorized <- ZIO.serviceWithZIO[SignOn](_.authenticate(from, userInput))
            _          <- ZIO.serviceWithZIO[Messager](_.send(authorized, to, message))
        yield ()

    override val run =
        program("Yura", "Ded Moroz", "Happy New Year!")
            .provide(
                UserRepo.layer,
                UserId.layer,
                SignOn.layer,
                Esia.layer,
                Messager.layer,
                ZLayer.Debug.mermaid
            )

//