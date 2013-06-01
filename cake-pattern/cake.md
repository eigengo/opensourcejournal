#Cake Pattern in Depth
This article is designed to serve as a “Cake pattern for dummies” walkthrough, primarily aimed at devs new(ish) to Scala – but with at least some prior Java (or similar) experience.

The Cake pattern can be (loosly) considered as Scalas answer to dependency injection (DI), you can see a comparison between Java Spring and the Cake Pattern on Jan Machacek’s blog by visiting cakesolutions.net. 
The basic application of DI and the Cake pattern solve the same thing: dependencies between components. There are subtle differences, but let’s discuss the mechanism of the Cake pattern rather than getting into the nitty gritty.

#Traits
First off, I think a quick overview of traits is necessary considering the target audience. Traits are similar to interfaces in Java, the main difference though is that they can be partially implemented, like abstract classes. However, unlike abstract classes, multiple traits can be ‘mixed in’ to a Scala class, allowing for a form of multiple inheritance. A trait cannot itself be instantiated, instead the traits functionality is mixed in either when defining or using a class. The syntax of Scala can make it look like you’re instantiating a trait, but in fact it’s creating an anonymous class with the trait mixed in, for example take the trait in Listing 1:

#####Listing 1. A simple trait
```scala
trait SimpleTrait {
  def addOne(x: Int): Int = {x+1}
}
```
We can instantiate this trait in two ways, the long hand way where we manually create a class and instantiate that; as per Listing 2:

#####Listing 2. Instantiating a trait, the long way
```scala
class SimpleClass extends SimpleTrait
val addOne = new SimpleClass
```
Or the shorthand way where an anonymous class is implicitly created for us in Listing 3:

#####Listing 3. Shorthand Trait instantiation
```scala
val addOne = new SimpleTrait{}
```
#And on we go
We’re going to write a simple application for saving and retrieving User objects. So, first we need to define our repository. We’ll start off with the API definition, then move onto the implementation of this.

#####Listing 4. Repository API definition
```scala
trait UserRepositoryComponent {
  def userLocator : UserLocator
  def userUpdater : UserUpdater
  trait UserLocator {
    def findAll: java.util.List[User]
  }
  trait UserUpdater {
    def save(user: User)
  }
}
```
Above we have a trait ``UserRepositoryComponent`` which is best described as a wrapper or container for all repository traits containing user related operations. Lets explain this line by line to make sure everything is clear:

* ``def userLocator: UserLocator`` – This defines a ``userLocator`` function which returns an instantiated ``UserLocator`` (trait), giving us a way to access it. We could also use a ``val`` here which instantiates a ``UserLocator`` directly, but this would reduce the flexibility of our implementation. By using ``def``, we can provide different subtypes of the dependency (``UserLocator``) by mixing in different traits. Furthermore, the caller of the ``UserLocator`` would be aware of the actual type due to the use of self-type annotations (described soon!). Using a val instead would not allow this subtyping.
* ``trait UserLocator{..}`` – This inner trait simply defines a function for retrieving a ``List`` of ``User`` objects. No implementation is yet provided.
* The ``UserUpdater`` definitions work in exactly the same way as those of the ``UserLocator``.

Right, so we’ve defined what we want our repository to do so I guess it’s time to implement it!

#####Listing 5. Repository JPA implementation
```scala
trait UserRepositoryJPAComponent extends UserRepositoryComponent {
  val em: EntityManager
  def userLocator = new UserLocatorJPA(em)
  def userUpdater = new UserUpdaterJPA(em)

  class UserLocatorJPA(val em: EntityManager) extends UserLocator {
    def findAll = em.createQuery("from User", classOf[User]).getResultList
  }
  class UserUpdaterJPA(val em: EntityManager) extends UserUpdater {
    def save(user: User) { em.persist(user) }
  }
}
```
This is fairly simple, but let’s walk through it step by step (half of it anyway, I’ll ignore the ``UserUpdater`` as it’s pretty much the same as the ``UserLocator`` stuff):

* ``val em: EntityManager`` – This simply defines a constant, ``EntityManager``. This ``val`` will be instantiated later when everything is tied up. As a result of using a ``val`` for this the value cannot change once assigned.
* ``def userLocator = new UserLocatorJPA(em)`` – Although this looks like a variable/field definition, it’s actually the implementation of the function defined in the previous trait. This implementation accepts an ``EntityManager`` and returns an instantiated ``UserLocatorJPA``.
* ``class UserLocatorJPA(val em: EntityManager) extends UserLocator{..}`` – A tiny bit more complex, we’re defining a class which implements the ``UserLocator`` trait. This class requires a JPA ``EntityManager`` to perform the various database operations.

#Abstracting away…
Next up we need to implement the service tier which will take advantage of the above repository. This will again consist of two traits, the first will define the interface and the second the implementation.

#####Listing 6. Service API
```scala
trait UserServiceComponent {
  def userService: UserService

  trait UserService {
    def findAll: java.util.List[User]
    def save(user: User)
  }
}
```
This is very similar to the ``UserRepositoryComponent`` trait defined previously and so should be familiar. We’ll move straight on to the implementation:

#####Listing 7. Service Implementation
```scala
trait DefaultUserServiceComponent extends UserServiceComponent {
  this: UserRepositoryComponent =>

  def userService = new DefaultUserService

  class DefaultUserService extends UserService {
    def findAll = userLocator.findAll

    def save(user: User) {
      userUpdater.save(user: User)
    }
  }
}
```
There are a couple of interesting things happening here so we’ll walk through this bit by bit:

* ``this: UserRepositoryComponent =>`` – This is the important bit and is called a self-type annotation, loosely speaking it means that this trait depends upon an implementation of the ``UserRepositoryComponent`` being injected. What this means is simple terms is that when we want to use/instantiate the ``DefaultUserServiceComponent``, we need to use the with keyword and specify an implementation of the ``UserRepositoryComponent``, which in this case would be  ``UserRepositoryJPAComponent``. This will be shown when we wire the whole thing together in the next step.
* Hopefully the rest of the code should be pretty clear, we’re simply defining a class which implements the UserServiceComponent and defines implementations for the two methods – simply delegating the calls to the two classes defined in the ``UserRepositoryComponent`` which we now have access to thanks to the self-type annotation

#Wiring everything up
Now we’ve got everything defined and implemented, we need a way of instantiating and obtaining access to the services. The way we do this is by defining a singleton object:

#####Listing 8. Application wrapper
```scala
object ApplicationLive {
  val userServiceComponent = 
    new DefaultUserServiceComponent with UserRepositoryJPAComponent {   
    val em = Persistence.createEntityManagerFactory("cake.pattern").
                     createEntityManager() 
  }
  val userService = userServiceComponent.userService
}
```

We first define and instantiate a ``UserServiceComponent``, mixing in the appropriate repository implementation, as required by the self-type annotation described earlier. We then provide the ``UserRepositoryJPAComponent`` implementation for instantiating the ``EntityManager``. Finally we define a ``userService val`` for accessing the service methods.

All that’s left is actually invoking the service methods, that can be done by placing the following code within your class:

#####Listing 9. Invoking services
```scala
val userService = ApplicationLive.userService
userService.findAll
```
*Simple yeah?*
#Testing
To test these services we’re going to write a Specs2 integration specification:

#####Listing 10. Test specifications!
```scala
class CakeTestSpecification extends Specification with Mockito {
  trait MockEntitManager {
    val em = mock[EntityManager]

    def expect(f: (EntityManager) => Any) {
      f(em)
    }
  }

  "findAll should use the EntityManager's typed queries" in {
    val query = mock[TypedQuery[User]]
    val users: java.util.List[User] = new ArrayList[User]()

    val userService = new DefaultUserServiceComponent
                        with UserRepositoryJPAComponent
                        with MockEntitManager
    userService.expect { em=>
      em.createQuery("from User", classOf[User]) returns query
      query.getResultList returns users
    }

    userService.userService.findAll must_== users
  }
}
```

What we’ve done here is mock out the ``EntityManager`` using a trait, whilst allowing the callee to pass a function to the trait to initialise the mock according to the specific requirements of the test. In our example this means mocking the ``createQuery.getResultList`` function to return an empty ``List``. A similar pattern could be used for mocking out the whole repository or service traits. The implementation details of this test Specs2 specification are beyond the scope of this article, however it will hopefully give you a head start when writing your own tests.

#Summary
There you have it, you can find the complete code on my github page (markglh). You can also follow me on twitter: @markglh.

#References
1. Real-World Scala: Dependency Injection (DI) by Jonas Bonér
http://jboner.github.com/2008/10/06/real-world-scala-dependency-injection-di.html
2. Dependency Injection in Scala: Extending the Cake Pattern by Adam Warski
http://www.warski.org/blog/2010/12/di-in-scala-cake-pattern/
3. Scalable Component Abstractions by Martin Odersky
http://lampwww.epfl.ch/~odersky/papers/ScalableComponent.pdf
