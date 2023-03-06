package com.rockthejvm
import scala.util.control.Breaks._

object linearsearch extends App{
  var Arr = Array(11, 12, 13, 14, 15)
  var i: Int = 0
  var item: Int = 0
  var position: Int = 0

  print("Enter item: ");
  item = scala.io.StdIn.readInt();

  breakable {
    position = -1
    while (i < Arr.size) {
      if (Arr(i) == item) {
        position = i;
        break;
      }
      i = i + 1
    }
  }
  if (position >= 0)
    printf(s"Item found at index: $position");
  else
    printf("Item not found\n");
}
