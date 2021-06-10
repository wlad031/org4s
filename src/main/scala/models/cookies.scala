package dev.vgerasimov.scorg
package models

object cookies {

  sealed trait StatCookie

  object StatCookie {

    case object EmptyPercent extends StatCookie
    case object EmptyFractional extends StatCookie

    case class Percent(value: Int) extends StatCookie {
      require(Percent.isValid(value), s"Invalid percent stat cookie value: $value")
    }

    object Percent {
      def isValid(percent: Int): Boolean = 0 <= percent && percent <= 100
    }

    case class Fractional(amount: Int, total: Int) extends StatCookie {
      require(
        Fractional.isValid(amount, total),
        s"Invalid values of fractional stat cookie: amount=$amount, total=$total"
      )
    }

    object Fractional {
      def isValid(amount: Int, total: Int): Boolean = 0 <= amount && 0 <= total && amount <= total
    }
  }
}
