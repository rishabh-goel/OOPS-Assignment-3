import com.rishabh.hw3.Computation.{BasicType, accessMap, attrMap, inheritanceMap, macroMap, nestedClassMap, objectMap, scopeMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import com.rishabh.hw3.Computation.SetExp.*

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Set}

class ComputationTest extends AnyFlatSpec with Matchers{

  // Test 1
  it should "invoke method inherited from abstract class" in {
    // Create an abstract and concrete class
    AbstractClassDef("AbstractClass1", Private(Field("f1")), Public(CreateMethod("m1", Params("a", "b"))), Public(CreateMethod("m2", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()
    ClassDef("MyClass1", Private(Field("f1")), Constructor(Assign("f1", Value(5))), Public(CreateMethod("m1", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()

    // Concrete class inherits from Abstract Class
    ClassDef("MyClass1") Extends AbstractClassDef("AbstractClass1")

    // Create and object of Concrete class
    NewObject("MyClass1", Variable("obj1"), Params("f1"), ListBuffer(Value(50))).eval()

    // Invoke the method of Abstract Class and check its value
    Assign("result1", InvokeObject(Value("MyClass1"), Value("obj1"), Value("m2"), Value(Set(1,2)), Value(Set(3,4)))).eval()
    assert(Variable("result1").eval() === mutable.HashSet(1, 2, 3, 4))
  }

  // Test 2
  it should "check that interfaces have no method body" in {

    // Creating an interface with method body should throw an exception
    val caught = intercept[Error] {
      Interface("MyErrorInterface", Public(CreateMethod("m1", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    }
    assert(caught.getMessage === "Interface methods can't have body")
  }

  // Test 3
  it should "check that a class is able to implement interface methods" in {

    // Create a Concrete Class and an Interface
    ClassDef("TestClass1", Private(Field("f1"))).eval()
    Interface("MyInterface1", Public(CreateMethod("m4", Params("a")))).eval()

    // An error should be thrown when the class implements the interface because there is no method created for the class
    val caught_impl = intercept[Error] {
      ClassDef("TestClass1") Implements Interface("MyInterface1")
    }
    assert(caught_impl.getMessage === "Class has to implement interface methods")

    // Create a Concrete Class and an Interface
    Interface("MyInterface", Public(CreateMethod("m4", Params("a")))).eval()
    ClassDef("TestClass2", Private(Field("f1")), Public(CreateMethod("m4", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    // An error should be thrown when the class implements the interface because the class is trying to implement the interface method with different number of parameters
    val caught_param = intercept[Error] {
      ClassDef("TestClass2") Implements Interface("MyInterface")
    }
    assert(caught_param.getMessage === "Cannot implement method from interface as method parameters don't match")
  }

  // Test 4
  it should "check the correct functionality of Implements and Extends feature" in {

    // Create a Concrete Class, an Abstract Class and an Interface
    AbstractClassDef("AbstractClass1", Private(Field("f1")), Public(CreateMethod("m1", Params("a", "b"))), Public(CreateMethod("m2", Params("a", "b"), Assign("c", Union(Variable("a"), Variable("b"))), Variable("c")))).eval()
    ClassDef("MyClass1", Private(Field("f1")), Constructor(Assign("f1", Value(5))), Public(CreateMethod("m1", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))).eval()
    Interface("MyInterface", Public(CreateMethod("m4", Params("a", "b")))).eval()

    // An error should be thrown when the class implements an abstract class
    val caught_impl1 = intercept[Error] {
      ClassDef("MyClass1") Implements AbstractClassDef("AbstractClass1")
    }

    // An error should be thrown when the interface implements a class
    val caught_impl2 = intercept[Error] {
      Interface("MyInterface") Implements ClassDef("MyClass1")
    }

    // An error should be thrown when the class tries to implement itself
    val caught_impl3 = intercept[Error] {
      ClassDef("MyClass1") Implements ClassDef("MyClass1")
    }

    assert(caught_impl1.getMessage === "A class can't be implemented. It can only be extended")
    assert(caught_impl2.getMessage === "Only a class/abstract_class can implement an interface")
    assert(caught_impl3.getMessage === "A class can't be implemented. It can only be extended")
  }

  // Test 5
  it should "check that multiple inheritance isn't allowed" in {

    // Create 3 interfaces
    Interface("MyInterface1", Public(CreateMethod("m4", Params("a", "b")))).eval()
    Interface("MyInterface2", Public(CreateMethod("m1", Params("a", "b", "c")))).eval()
    Interface("MyInterface3", Public(CreateMethod("m2", Params("a", "b")))).eval()

    // Should succeed as MyInterface3 inherits another interface for the first time
    Interface("MyInterface3") Extends Interface("MyInterface2")

    // An error should be thrown as multiple inheritance is not supported
    val caught_extd = intercept[Error] {
      Interface("MyInterface3") Extends Interface("MyInterface1")
    }
    assert(caught_extd.getMessage === "Cannot support multiple inheritance")
  }
}
