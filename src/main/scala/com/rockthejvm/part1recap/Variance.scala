package com.rockthejvm.part1recap

import java.util

object Variance  {

  // OOP - substitution
  class Animal
  class Dog(name:String) extends Animal

  // Variance question for List: if Dog is a subtype of Animal, then should List[Dog] be a subtype of List[Animal]?

  //  If the answer is yes, then it is a COVARIANT

  val lassie = new Dog("Lassie")
  val hatchi = new Dog("Hatchi")
  val laika = new Dog("Laika")

  val anAnimal: Animal = lassie

  val someAnimal: List[Animal] = List(lassie, hatchi, laika)
  //this is a list of animal AND a list of dog- both makes sense

  class MyList[+A] //plus sign denotes that the type is a covariant. MyList is COVARIANT in A.
  val myAnimalList: MyList[Animal] = new MyList[Dog]

  //if no, then the type is INVARIANT

  //two semigroups of different types have no subtype relationship- so it's not covariant:
  trait SemiGroup[A] {
    def combine(x: A, y: A): A
  }

  //all generics in Java are invariants. There is no link between Animal and Dog types in the java language.
  // val aJavaList: java.util.ArrayList[Animal] = new util.ArrayList[Dog]()

  // if the answer is ABSOLUTELY NOT: contravariants.
  trait Vet[-A] {
    def heal(animal:A): Boolean
  }

  //Vet[Animal] is "better" than a Vet[Dog] because it can treat any animal, therefore my dog as well.
  //inverse subtyping:
  //Because Dog <:(subtype) Animal, then Vet[Dog] >:(supertype) Vet[Animal]
  val myVet: Vet[Dog] = new Vet[Animal] {
    override def heal(animal: Animal): Boolean = {
      println("Here you go, you're good now...")
      true
    }
  }

  val healingLassie = myVet.heal(lassie)

  /*
  * Rule of thumb:
  - If the type PRODUCES or RETRIEVES values of type A (e. g. lists), then the type should be COVARIANT
  - If the type CONSUMES or ACTS ON values of type A (e. g. a vet), then the type should be CONTRAVARIANT
  - Otherwise, invariant
  * */

  //only covariants have generic fields using their generic type.
  //demo of why this is the case:

  //  class Cat extends Animal
  // class Vet2[-A](val favoriteAnimal: A) <-- the types of val/var fields are in covariant position, wont compile

  //  val garfield = new Cat
  //  val theVet: Vet2[Animal] = new Vet2[Animal](garfield)
  //  val dogVet: Vet2[Dog] = theVet
  //  val favoriteAnimal: Dog = dogVet.favoriteAnimal <-- must be a Dog - type conflict!

  // ______________________________________________________________________

  // var fields are also in contravariant positions, so it can cause type conflicts:

  /*
  class MutableContainer[+A](var contents: A)

  val containerAnimal: MutableContainer[Animal] = new MutableContainer[Dog](new Dog)
  containerAnimal.contents = new Cat <-- type conflict because var contents can be reassigned!

  compiler is unable to enforce the rules of covariants:
    covariants must ensure that the type used is the same type as defined when constructed.
   */

  // ______________________________________________________________________

  // types of method arguments are in CONTRAVARIANT position:

  /*
  class MyList2[+A] {
    def add(element: A):MyList[A]
  } <-- add CONSUMES the argument, a behaviour specific to contravariant types.

  val animals: MyList2[Animal] = new MyList2[Cat]
  val biggerListOfAnimals: MyLift2[Animal] = animals.add(new Dog) <-- type conflict

   */

  //SOLUTION: WIDEN THE TYPE ARG

  abstract class MyList2[+A] {
    def add [B >: A](element: B): MyList[B]
  }

  // _________________________________________________________________________

  //method return types are in COVARIANT position bco production, so this wont compile:

  /*
     abstract class Vet2[-A] {
      def rescueAnimal(): A
    }

  val vet: Vet2[Animal] = new Vet2[Animal] {
  def rescueAnimal(): Animal = new Cat

  val lassieVet: Vet2[Dog] = vet
  val rescureDog: Dog = lassieVet.rescueAnimal() // must return a Dog, but it returns a Cat <-- type conflict!
   */

  //SOLUTION: NARROW THE RETURN TYPE

  abstract class Vet2[-A] {
    def rescueAnimal[B <: A](): B
  }


  def main(args: Array[String]): Unit = {

  }
}
