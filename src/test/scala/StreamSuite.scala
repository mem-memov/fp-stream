class StreamSuite extends munit.FunSuite:

  test("Stream gets converted to list") {

    val stream = Stream(1, 2, 3)
    val list: List[Int] = stream.toList

    assert(list == List(1, 2, 3))
  }

  test("Stream gives n elements") {

    val stream = Stream(1, 2, 3)
    val headStream = stream.take(2)
    val headList = headStream.toList

    assert(headList == List(1, 2))
  }
