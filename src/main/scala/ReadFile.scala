import scala.collection.mutable
import scala.io.Source

class ReadFile {
  def readFieldDescr(str: String) = {

  }


  def readLus(fileName: String): mutable.LinkedHashMap[Int, TupleTree]
  = {
    var lusTreeMap = new mutable.LinkedHashMap[Int, Array[String]]()
    for (line <- Source.fromFile(fileName, "windows-1251").getLines()) {
      val lusMap = line.split(";")
      if (lusMap(0).equals("F339")) {
        lusTreeMap.+=(Integer.parseInt(lusMap(1)) -> lusMap)
      }
    }
    parseLusMapToTree(lusTreeMap)
  }


  def parseLusMapToTree(lusTreeMap: mutable.LinkedHashMap[Int, Array[String]]): mutable.LinkedHashMap[Int, TupleTree] = {
    var lusMap = new mutable.LinkedHashMap[Int, TupleTree]
    for (key <- lusTreeMap.keySet) {
      if (lusTreeMap.keySet.contains(key)) {
        val lusTree = new TupleTree(key, lusTreeMap.get(key).get)
        if (lusTreeMap.get(key).get(7) != null && Integer.parseInt(lusTreeMap.get(key).get(7)) != 0) {
          lusTree.left = new TupleTree(Integer.parseInt(lusTreeMap.get(key).get(7)), lusTreeMap.get(key).get)
        }
        if (lusTreeMap.get(key).get(9) != null && Integer.parseInt(lusTreeMap.get(key).get(9)) != 0) {
          lusTree.right = new TupleTree(Integer.parseInt(lusTreeMap.get(key).get(9)), lusTreeMap.get(key).get)
        }
        lusMap.+=(key -> lusTree)
      }
    }

    var delKeys = new mutable.HashSet[Int]()
    for (key <- lusMap.keySet) {
      for (tree <- lusMap.valuesIterator) {
        if (tree.left != null) {
          if (tree.left._rootElem._1 == key) {
            println(tree.left.elem + " is Left child YES of " + tree.elem)
            lusMap.get(tree.elem._1).get.left = lusMap.get(key).get
            delKeys.add(key)
          }
        }
        if (tree.right != null) {
          if (tree.right._rootElem._1 == key) {
            println(tree.right.elem + " is Right child NO of " + tree.elem)
            lusMap.get(tree.elem._1).get.right = lusMap.get(key).get
            delKeys.add(key)
          }
        }
      }
    }
    for (key<-delKeys){
      lusMap.remove(key)
    }
    lusMap
  }
}





