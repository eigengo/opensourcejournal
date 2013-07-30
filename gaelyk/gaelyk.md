#Your Website in the Cloud with Gaelyk

Gaelyk is lightweight toolkit for creating websites in the cloud easily. 
It can power your personal blog as well as serve millions of users a day. 
Thanks to underlying Google App Engine platform it really excels in situations 
when you have to deal with multi tenant databases, image manipulation, 
geospatial or text searches. Groovy as a language of choice on the other hand allows you 
to write less boilerplate and being more productive.

##Groovy Websites with Sugar
Gaelyk builds on the top of standard Groovy support for writing websites. 
The basic unit of work is *groovlet* which is script handling requests to particular URL. 
The script has access to some objects such as request, response or map of request parameters. 
Gaelyk adds more bindings into the script such as datastore or user service.

######Listing 1. Hello world groovlet - helloWorld.groovy
```groovy
out << "Hello World"
```

To create simple *Hello World* website you place script into ```WEB-INF/groovy``` folder of web application directory. 
When you run the server your groovlet is executed when you access 
```http://localhost:8080/helloWorld.groovy``` URL, assuming 8080 is your default server port.

Of course you usually don't want to render plain text in your application. 
In modern web pages you probably want to render JSON responses for your AJAX request. 
Groovy groovlets has built in JSON builder you can use to communicate with your client side JavaScript code.

######Listing 2. JSON groovlet - helloJson.groovy
```groovy
json(message: params.message ? "Hello $params.message" : "Hi!")
response.contentType = 'application/json'
out << json
```

Also for old school web pages there are *Groovy Templates* for HTML templating. 
As in for example Java Server Pages you can use ```<% %>``` and ```<%= %>``` scriptlets 
to execute code such as conditions within the template. In addition you can use 
```$request.attribute``` or ```${request.attribute}``` to access objects from the request attributes map. 
The usual workflow is pretty obvious -  in groovlet you fill the data and then you render them in the template.

######Listing 3. Hello world groovlet - helloWorldWithRequest.groovy
```groovy
request.message = params.message
forward '/helloWorld.gtpl'
```

######Listing 4. Hello world template - helloWorld.gtpl
```jsp
<html>
    <head>
        <title><%= request.message ?: "Hello World" %></title>
    </head>
    <body>
        <% if(request.message) { %>
        <h1>$request.message</h1>
        <% } else { %>
        <h1>No Message</h1>
        <% } %>
    </body>
</html>
```

Having ```.groovy``` extension in the URL isn't very good practise 
so Gaelyk adds yet another feature beside groovlets and templates to create SEO friendly URLs. 
In file ```WEB-INF/routes.groovy```, you define so called routes which maps particular URL to groovlets or templates.
You declare HTTP method, URL pattern which may contain URL variables starting with *at* sign (```@```), 
the destination and the action (forward or redirect).

######Listing 5. Routes definition - routes.groovy
```groovy
get '/',            forward: '/helloWorld.groovy'
get '/hello/@what', forward: '/helloWorld.groovy?message=@what'
get '/article/@year?/@month?/@day?/@title?', 
                    forward: '/article.groovy'
```

There is lot more options you can use in routing such as validation. To see more read Flexible URL Routing section of Gaelyk Tutorial.

##Using platform services
Gaelyk simplifies access to many underlying services such as mail, cache, datastore or search. 
Let's focus on the last two.

###Saving data
Nearly every web application needs to store some data. Gaelyk reuses Google App Engine's 
term *entity* to to refer to objects stored into underlying datastore. 
Defining an entity is simple process of adding ```@Entity``` annotation to the plain old groovy object.

######Listing 6. Example entity - WordBlogPost.groovy
```groovy
import groovyx.gaelyk.datastore.*
import com.google.appengine.api.datastore.*

@Entity class WordBlogPost {
    @Indexed Long authorId
    @Indexed Date created = new Date()
    @Indexed List<String> tags

    String title
    String body
    GeoPoint location
}
```

As Google App Engine's default option for storing data is schemaless Google Cloud Datastore (aka BigTable).
You need to pay attention to few details when using this service. 
If you're going to sort or query using some entity property, 
you need to mark it as indexed using ```@Indexed``` annotation.

```@Entity``` annotation is internally implemented Groovy AST transformation 
which adds several useful methods such as ```save()```, ```delete()```, ```count()``` and ```findAll()```
to the annotated class. In following example you can see these methods in action:

######Listing 7. Using entity methods
```groovy
// create a new blog post
def blogPost = new WordBlogPost(
    authorId: 1, 
    tags: ['groovy', 'gaelyk'], 
    title: 'Your website in the cloud with Gaelyk',
    body: 'Gaelyk is lightweight toolkit for ...',
    location: new GeoPoint(50, 14)
)

// unless saved the id property is not initialized
assert !blogPost.id

// saving new blog post to the data store
blogPost.save()
// now the id is initialized
assert blogPost.id

// you could later simply fetch the blog post by its id
assert blogPost == WordBlogPost.get(blogPost.id)

// you can query and sort the blog posts using the query dsl
assert WordBlogPost.findAll {
    where authorId == 1
    and tag == groovy
    sort desc by created
}.size() == 1

// or delete the blog post
blogPost.delete()

// so we have no more blog posts
assert WordBlogPost.count() == 0
```

###Adding search capability to your website
Search is crucial for many web sites. For example in some traveler's blog you may want to perform search 
through the bodies and titles. Or sort the posts based on the distance to your current location. 
To do this, you need to add the your blog post entity to the search index as well. 

######Listing 8. Adding entity to the index
```groovy
search.index("posts").put {
    document(id: "$blogPost.id") {
        author text: blogPost.author.fullName
        title text: blogPost.text
        created date: blogPost.created
        body text: blogPost.body
        tags atom: blogPost.tags
        location geoPoint: [location.latitude, location.longitude]
    }
}
```

Simple word search is just question of calling search method on given index:

######Listing 9. Simple word search
```groovy
def fultextResults = search.index("posts").search("Hello")
```

For advanced use cases you can use search DSL provided by Gaelyk:

######Listing 10. Find closest blog posts
```groovy
def closestPosts = search.search {
    select ids
    from posts
    sort asc by distance(location, geopoint(lat,long)), defaultDistance
}
```


##Creating Your Own Gaelyk Website
There are currently at least two ways how to start developing your own website based on Gaelyk.
For small sites you can try new tool called Glide. 
Glide excels in hiding all the complexity which can occur in traditional web application 
and you should definitely give it a try even it doesn't support all the Gaelyk functionally yet. 
In this article, we are going to show you the traditional process of creating new Gaelyk application 
from the Gaelyk Template Project.

###Gaelyk Template Project
You can download Gaelyk Template Project from the Gaelyk website. 
Gaelyk uses yet another great Groovy tool Gradle as a build system so 
if you are familiar with Gradle you should quickly recognize template project structure. 
Actually the folder structure is similar to default Maven structure as well. 
Common project sources are placed in ```src/main/groovy``` directory. 
Groovlets reside in ```src/main/webapp/WEB-INF/groovy``` 
and templates can be placed anywhere inside ```src/main/webapp``` 
but usually they are stored in ```WEB-INF/pages``` directory within the web application root. 
```WEB-INF``` folder also contains some important files such as ```web.xml```, ```appengine-web.xml``` or ```routes.groovy```. 

######Listing 11. Gaelyk Template Project directory structure
```
├── build.gradle
└── src
    ├── functionalTest
    │   └── groovy
    │       └── SmokeSpec.groovy
    ├── main
    │   ├── groovy
    │   └── webapp
    │       └── WEB-INF
    │           ├── appengine-web.xml
    │           ├── groovy
    │           │   ├── datastoreGroovlet.groovy
    │           │   └── datetime.groovy
    │           ├── logging.properties
    │           ├── pages
    │           │   ├── datetime.gtpl
    │           │   └── index.gtpl
    │           ├── routes.groovy
    │           └── web.xml
    └── test
        └── groovy
            └── DatastoreGroovletSpec.groovy
```


To help you starting with your own project the template project already contains some example files 
such as ```datetime.groovy``` which stores current date into request object 
and forwards to the ```datetime.gtpl``` template which will render that information.

Gaelyk provides good support for unit and functional testing. 
Unit testing is based on brilliant Spock Framework and functional testing uses great Geb browser automation tool. 
Example unit test you can find in ```src/test/groovy``` and simple *smoke* test checking 
that the website is running you can find in ```src/functionalTest/groovy```.

###Running and deploying your website
Gradle GAE plugin used by Gaelyk Template Project 
provides all the tasks you need to run and deploy your application. 
To run your website locally, use ```./gradlew gaeRun``` task, to upload your website to the cloud 
use ```./gradlew gaeUpdate```.

There are no more configuration steps needed to get your website working 
in the local development environment other than having Java installed 
and ```JAVA_HOME``` environmental variable set up. 
The last mile is to upload your application to Google's servers. 
You need to obtain your application identifier first. Visit Google App Engine website, 
sign up for the services and create new application. 
Update the ```application``` tag in ```appengine-web.xml``` configuration file 
and run the ```./gradlew gaeUpdate``` task. 
When asked for your credentials supply the one from your Google account you have used to sign up for Google App Engine. 
After deployment, the website will be available online at ```http://<your-app-id>.appspot.com```.

## Summary

We have covered very basic examples how to create and deploy your website using Gaelyk.
For more examples and tutorial visit Gealyk's website or my blog Groovy in the Clouds.

## References
1. Gaelyk: a lightweight Groovy toolkit for Google App Engine Java by Guillaume Laforge http://gaelyk.appspot.com
2. Groovy: Groovlets http://groovy.codehaus.org/Groovlets
3. Groovy: Groovy Templates http://groovy.codehaus.org/Groovy+Templates
4. Google App Engine https://developers.google.com/appengine/
5. Gradle GAE Plugin by Benjamin Muschko https://github.com/bmuschko/gradle-gae-plugin
6. Glide by Kunal Dabir http://glide-gae.appspot.com
7. Groovy in the Clouds by Vladimir Orany http://vladimir.orany.cz
