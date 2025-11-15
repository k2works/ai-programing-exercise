package api.support

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import io.circe.{Decoder, Encoder, Printer}
import io.circe.parser.decode

import scala.concurrent.Future

/**
 * Circe を使った JSON マーシャリングのサポート
 *
 * akka-http-circe の代替実装
 */
trait JsonSupport {

  private val printer: Printer = Printer.noSpaces

  /**
   * Encoder を使って JSON にマーシャリング
   */
  implicit def circeToEntityMarshaller[A](implicit encoder: Encoder[A]): ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(ContentTypes.`application/json`) { a =>
      HttpEntity(ContentTypes.`application/json`, printer.print(encoder(a)))
    }

  /**
   * Decoder を使って JSON からアンマーシャリング
   */
  implicit def circeFromEntityUnmarshaller[A](implicit
    decoder: Decoder[A]
  ): FromEntityUnmarshaller[A] =
    Unmarshaller.stringUnmarshaller
      .forContentTypes(ContentTypes.`application/json`)
      .flatMap(_ => _ => json => decode[A](json).fold(Future.failed, Future.successful))

}

object JsonSupport extends JsonSupport
