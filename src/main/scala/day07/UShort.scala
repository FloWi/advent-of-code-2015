package day07

object UShort {
  @inline final def apply(n: Char): UShort = new UShort(n)
  @inline final def apply(n: Short): UShort = new UShort(n.toChar)
  @inline final def apply(n: Int): UShort = new UShort(n.toChar)

  @inline final def MinValue: UShort = UShort(0)
  @inline final def MaxValue: UShort = UShort(Char.MaxValue)
}

class UShort(val signed: Char) extends AnyVal {
  def toByte: Byte = signed.toByte
  def toChar: Char = signed
  def toShort: Short = signed.toShort
  def toInt: Int = signed.toInt
  def toLong: Long = signed.toLong
  def toFloat: Float = signed.toFloat
  def toDouble: Double = signed.toDouble
  def toBigInt: BigInt = BigInt(toInt)

  def isValidByte: Boolean = signed == toByte
  def isValidShort: Boolean = signed == toShort
  def isValidChar: Boolean = true
  def isValidInt: Boolean = true
  def isValidLong: Boolean = true

  override def toString: String = toInt.toString

  def ==(that: UShort): Boolean = this.signed == that.signed
  def !=(that: UShort): Boolean = this.signed != that.signed

  def ===(that: UShort): Boolean = this.signed == that.signed
  def =!=(that: UShort): Boolean = this.signed != that.signed

  def <=(that: UShort): Boolean = this.signed <= that.signed
  def <(that: UShort): Boolean = this.signed < that.signed
  def >=(that: UShort): Boolean = this.signed >= that.signed
  def >(that: UShort): Boolean = this.signed > that.signed

  def unary_- : UShort = UShort(-this.signed)

  def +(that: UShort): UShort = UShort(this.signed + that.signed)
  def -(that: UShort): UShort = UShort(this.signed - that.signed)
  def *(that: UShort): UShort = UShort(this.signed * that.signed)
  def /(that: UShort): UShort = UShort(this.signed / that.signed)
  def %(that: UShort): UShort = UShort(this.signed % that.signed)

  def unary_~ : UShort = UShort(~this.signed)

  def <<(shift: Int): UShort = UShort((signed & 0xffff) << (shift & 15))
  def >>(shift: Int): UShort = UShort((signed & 0xffff) >>> (shift & 15))
  def >>>(shift: Int): UShort = UShort((signed & 0xffff) >>> (shift & 15))
  def &(that: UShort): UShort = UShort(this.signed & that.signed)
  def |(that: UShort): UShort = UShort(this.signed | that.signed)
  def ^(that: UShort): UShort = UShort(this.signed ^ that.signed)

}
