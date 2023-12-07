/*
 * Copyright 2023 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.otel4s.middleware

import java.io.ByteArrayOutputStream
import java.io.FilterOutputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.util.Using

private[middleware] object helpers {
  def printStackTrace(e: Throwable): String = {
    val baos = new ByteArrayOutputStream
    Using.resource(new AnsiFilterStream(baos)) { fs =>
      Using.resource(new PrintStream(fs, true, "UTF-8")) { ps =>
        e.printStackTrace(ps)
      }
    }
    new String(baos.toByteArray, "UTF-8")
  }

  private final case class State(apply: Int => State)

  /** Filter ANSI codes out of an OutputStream. */
  private class AnsiFilterStream(os: OutputStream) extends FilterOutputStream(os) {

    val S: State = State {
      case 27 => I0
      case _ => F
    }

    val F: State = State(_ => F)

    val T: State = State(_ => T)

    val I0: State = State {
      case '[' => I1
      case _ => F
    }

    val I1: State = State {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => I2
      case _ => F
    }

    val I2: State = State {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => I2
      case ';' => I1
      case '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
          'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\' | ']' |
          '^' | '_' | '`' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' |
          'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '{' |
          '|' | '}' | '~' =>
        T // end of ANSI escape
      case _ => F
    }

    // Strategy is, accumulate values as long as we're in a non-terminal state, then either discard
    // them if we reach T (which means we accumulated an ANSI escape sequence) or print them out if
    // we reach F.

    private var stack: List[Int] = Nil
    private var state: State = S // Start

    override def write(n: Int): Unit =
      state.apply(n) match {

        case F =>
          stack.foldRight(())((c, _) => super.write(c))
          super.write(n)
          stack = Nil
          state = S

        case T =>
          stack = Nil
          state = S

        case s =>
          stack = n :: stack
          state = s

      }

  }
}
