
object Checkout extends App {

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

  def calculateNumberOfOccurances(products: List[Product]): Map[Char, Int] = {
    val listOfProducts = products.map(_.productId)
    listOfProducts.groupBy(l => l).map(t => (t._1, t._2.length))
  }

  def calculateCostForDiscountedItem(numberOfItems: Int, individualCost: Int, offers: Option[Offer]): Int = {

    val withDiscount = for {
    batchPrice <- offers.map(offer => offer.batchPrice)
    numNeededForDiscount <- offers.map(offer => offer.numberNeededForDiscount)
    } yield {
      val remainder = numberOfItems % numNeededForDiscount
      val numberIncludedInDiscount = (numberOfItems - remainder) / numNeededForDiscount
      (numberIncludedInDiscount * batchPrice) + (remainder * individualCost)
    }

    withDiscount match {
      case Some(value) => value
      case None => numberOfItems * individualCost
    }
  }

  def calculateTotalValueOfBasket(products: List[Product]): Unit = {
    val ItemsWithOccurances: Map[Char, Int] = calculateNumberOfOccurances(basket)

    val a = for {
//    priceForA <- products.map(product => calculateCostForDiscountedItem(ItemsWithOccurances('A'), product.unitPrice, product.offer))
//    priceForB <- products.map(product => calculateCostForDiscountedItem(ItemsWithOccurances('B'), product.unitPrice, product.offer))
    priceForC <- products.map(product => calculateCostForDiscountedItem(ItemsWithOccurances('C'), product.unitPrice, product.offer))
    } yield priceForC

    println(a)

  }

  calculateTotalValueOfBasket(basket)


  case class Product(productId: Char, unitPrice: Int, offer: Option[Offer])
  case class Offer(numberNeededForDiscount: Int, batchPrice: Int)
}
