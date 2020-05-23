object Checkout extends App {

  case class Product(productId: Char, unitPrice: Int, offer: Option[Offer])
  case class Offer(numberNeededForDiscount: Int, batchPrice: Int)

  def calculateNumberOfOccurances(products: List[Product]): Map[Char, Int] =
    products
      .groupBy(_.productId)
      .map { case (pid, instances) => (pid, instances.length) } // 1
      .withDefaultValue(0) // 2

  /* 1. You can pass a partial function to `map`
   * in order to destructure the tuple instead of using `tuple._1`
   * and `tuple._2`.
   *
   * 2. You can use `Map#withDefault` to provide a default
   * value when sending it a key that doesn't exist so it doesn't
   * throw `NoSuchElementException`.
   * E.g. `Map("One" -> 1)("Two")` will throw an exception.
   */

  def calculateCostForDiscountedItem(
      product: Product,
      numberOfItems: Int
  ): Int = product.offer match { // 3
    case None => product.unitPrice * numberOfItems
    case Some(offer) => {
      val excludedFromDiscount = numberOfItems % offer.numberNeededForDiscount
      val discountBatches = numberOfItems / offer.numberNeededForDiscount
      (excludedFromDiscount * product.unitPrice) + (discountBatches * offer.batchPrice)
    }
  }
  

  /* 3. Most of the time, a `match` is the cleanest way to
   * access values within `Option`s and `Either`s..
   * Getting used to the `map` and `flatMap` methods of
   * these types can also be very helpful.
   * For comprehensions are actually just syntactic sugar for
   * `map`, `filter`, and `flatMap`. See https://docs.scala-lang.org/tour/for-comprehensions.html
   */

  def calculateTotalValueOfBasket(products: List[Product]): Int = { // 4
    val itemOccurances: Map[Char, Int] =
      calculateNumberOfOccurances(products)

    products.distinct.map { product => // 5
      calculateCostForDiscountedItem(product, itemOccurances(product.productId))
    }.sum
  }

  /* 4. `calculateTotalValueOfBasket` is now a *pure function*. It used to
   * print the results to the screen, which is a side effect.
   * It also used to depend on the global value `basket`,
   * which made it less reusable.
   *
   * 5. We used `List#distinct` method to make sure that the
   * price of an item is not calculated multiple times.
   */

  val basket = List(
    Product('A', 50, Some(Offer(3, 130))),
    Product('B', 30, Some(Offer(2, 45))),
    Product('B', 30, Some(Offer(2, 45))),
    Product('C', 20, None),
    Product('C', 20, None),
    Product('C', 20, None),
    Product('A', 50, Some(Offer(3, 130))),
    Product('A', 50, Some(Offer(3, 130)))
  )

  // A fun trick:
  println(
    s"${Console.YELLOW}Total price:${Console.GREEN} ${calculateTotalValueOfBasket(basket)}${Console.RESET}"
  )

}
