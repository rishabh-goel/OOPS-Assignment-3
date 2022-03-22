import com.rishabh.hw3.Computation.{BasicType, accessMap, attrMap, inheritanceMap, macroMap, nestedClassMap, objectMap, scopeMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import com.rishabh.hw3.Computation.SetExp.*

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Set}

class ComputationTest extends AnyFlatSpec with Matchers{
  
  // Test 1
  it should "invoke method inherited from abstract class" in {
    AbstractClassDef("AbstractClass1", Private(Field("f1")), Public(CreateMethod("m1", Params("a", "b"))), Public(CreateMethod("m2", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()
    ClassDef("MyClass1", Private(Field("f1")), Constructor(Assign("f1", Value(5))), Public(CreateMethod("m1", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    ClassDef("MyClass1") Extends AbstractClassDef("AbstractClass1")
    NewObject("MyClass1", Variable("obj1"), Params("f1"), ListBuffer(Value(50))).eval()
    Assign("result1", InvokeObject(Value("MyClass1"), Value("obj1"), Value("m2"), Value(Set(1,2)), Value(Set(3,4)))).eval()
    assert(Variable("result1").eval() === mutable.HashSet(1, 2, 3, 4))
  }

  // Test 2
  it should "check that interfaces have no method body" in {

    val caught = intercept[Error] {
      Interface("MyErrorInterface", Public(CreateMethod("m1", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    }

    assert(caught.getMessage === "Interface methods can't have body")
  }

  // Test 3
  it should "check that a class is able to implement interface methods" in {

    ClassDef("TestClass1", Private(Field("f1"))).eval()
    Interface("MyInterface1", Public(CreateMethod("m4", Params("a")))).eval()
    val caught_impl = intercept[Error] {
      ClassDef("TestClass1") Implements Interface("MyInterface1")
    }
    assert(caught_impl.getMessage === "Class has to implement interface methods")

    Interface("MyInterface", Public(CreateMethod("m4", Params("a")))).eval()
    ClassDef("TestClass2", Private(Field("f1")), Public(CreateMethod("m4", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    val caught_param = intercept[Error] {
      ClassDef("TestClass2") Implements Interface("MyInterface")
    }
    assert(caught_param.getMessage === "Cannot implement method from interface as method parameters don't match")
  }

  // Test 4
  it should "check the correct functionality of Implements and Extends feature" in {
    // PnC between classes/abstract class and interface
    AbstractClassDef("AbstractClass1", Private(Field("f1")), Public(CreateMethod("m1", Params("a", "b"))), Public(CreateMethod("m2", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()
    ClassDef("MyClass1", Private(Field("f1")), Constructor(Assign("f1", Value(5))), Public(CreateMethod("m1", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    Interface("MyInterface", Public(CreateMethod("m4", Params("a", "b")))).eval()

    val caught_impl1 = intercept[Error] {
      ClassDef("MyClass1") Implements AbstractClassDef("AbstractClass1")
    }

    val caught_impl2 = intercept[Error] {
      Interface("MyInterface") Implements ClassDef("MyClass1")
    }

    val caught_impl3 = intercept[Error] {
      ClassDef("MyClass1") Implements ClassDef("MyClass1")
    }

    assert(caught_impl1.getMessage === "A class can't be implemented. It can only be extended")
    assert(caught_impl2.getMessage === "Only a class/abstract_class can implement an interface")
    assert(caught_impl3.getMessage === "A class can't be implemented. It can only be extended")
  }

  // Test 5
  it should "check that multiple inheritance isn't allowed" in {

    Interface("MyInterface1", Public(CreateMethod("m4", Params("a", "b")))).eval()
    Interface("MyInterface2", Public(CreateMethod("m1", Params("a", "b", "c")))).eval()
    Interface("MyInterface3", Public(CreateMethod("m2", Params("a", "b")))).eval()

    Interface("MyInterface1") Extends Interface("MyInterface2")
    val caught_extd = intercept[Error] {
      Interface("MyInterface1") Extends Interface("MyInterface3")
    }
    assert(caught_extd.getMessage === "Cannot support multiple inheritance")
  }
}
