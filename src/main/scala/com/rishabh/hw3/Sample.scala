import com.rishabh.hw3.Computation.*
import com.rishabh.hw3.Computation.SetExp.*
import scala.collection.mutable.*
object Sample extends App {

  ClassDef("MyClass", Public(Field("f1")), Constructor(Assign("f1", Value(5))), Public(CreateMethod("m1", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()
  ClassDef("MyInner", Public(Field("e1")), Constructor(Assign("e1", Value(100))), Private(CreateMethod("m2", Params("a", "b"), Assign("c", Intersect(Variable("a"), Variable("b"))), Variable("c")))).eval()
  InnerClass("MyClass", "MyInner").eval()
  NewObject("MyClass", Variable("obj1"), Params("f1"), ListBuffer(Value(50))).eval()
  NewObject("MyClass", Variable("obj2"), Params("f1"), ListBuffer(Value(15))).eval()
  NewObject("MyInner", Variable("innerObj1"), Params("e1"), ListBuffer(Value(0)), "obj1").eval()
  NewObject("MyInner", Variable("innerObj2"), Params("e1"), ListBuffer(Value(20)), "obj1").eval()
  ClassDef("DerivedClass", Public(Field("d1")), Private(Field("d2", Value(2))), Protected(Field("d3", Value(5))), Protected(Field("d4")), Constructor(Assign("d1", Value(10)))).eval()
  ClassDef("DerivedClass") Extends ClassDef("MyClass")
  NewObject("DerivedClass", Variable("derivedObj"), Params("d1"), ListBuffer(Value(20))).eval()
  println(scopeMap("MyClass"))
  println(scopeMap("DerivedClass"))
  //    println(objectMap)
  //    println(attrMap)
  println(InvokeObject(Value("MyClass"), Value("obj1"), Value("m1"), Value(Set(1,2)), Value(Set(3,4))).eval())
  println(InvokeObject(Value("MyInner"), Value("innerObj1"), Value("m2"), Value(Set(1,2)), Value(Set(1,4))).eval())
      println(scopeMap)
      println(objectMap)
      println(attrMap)

  println(accessMap("public"))
  println(accessMap("private"))
  println(accessMap("protected"))
  println(inheritanceMap)
  println(nestedClassMap)





}