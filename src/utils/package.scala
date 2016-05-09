/**
 * Some useful functions for Scala.
 *
 * @author Pierre-André Mudry
 * @version 1.0
 */

import java.io.BufferedOutputStream
import scala.io.Source
import java.io.File
import java.io.FileOutputStream
import scala.io.Codec
package object utils {

	/**
	 * There are functions for micro-benchmarking, but you should not
	 * rely on those functions for measuring short durations!
	 */

	// Measure the time for a block of code to run, approximately
	def timeVerbose(f: ⇒ Unit) = {
		println("[Time] Start of measure")
		val duration = time(f)
		println(s"[Time] Block duration was $duration ms\n")
		duration
	}

	// With a customized error message
	def timeVerbose(s: String)(f: ⇒ Unit) = {
		println(s"[$s] Start of measure")
		val duration = time(f)
		println(s"[$s] Block duration was $duration ms\n")
		duration
	}

	// Measure the time for a block of code to run, approximately
	def time(f: ⇒ Unit) = {
		val start = System.currentTimeMillis
		f // Execute the block
		val duration = System.currentTimeMillis - start
		duration
	}

	/**
	 * A class for reading and writing to files easily
	 */
	implicit class RichFile(file: File) {
		def read() = Source.fromFile(file)(Codec.UTF8).mkString

		def write(data: String) {
			val fos = new BufferedOutputStream(new FileOutputStream(file))
			try {
				fos.write(data.getBytes("UTF-8"))
			} finally {
				fos.close
			}
		}
	}
}