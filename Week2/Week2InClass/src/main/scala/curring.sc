def sum(f: Int => Int) : (Int, Int) => Int = {
  def sumAux(a: Int, b: Int) : Int =
  if (a > b) 0
  else f(a) + sumAux(a+1,b)
  sumAux
}

def sumIdentity = sum(x => x)
def sumCube = sum((x: Int) => x * x * x)
def sumSquare = sum(x => x * x)

sumIdentity(1,3)
sumCube(1,3)
sumSquare(1,3)