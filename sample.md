#Abstract
The Open Source Journal sample article sets out to show the overall structure of an ideal OSJ article: its language & tone as well as its preferred style (in the markdown format).    
Typical OSJ article is approximately 1000-4000 words; if you article falls significantly outside this range, please get in touch to see whether it will fit the OSJ.

#Introduction
Introductio sets out the scene for the article. It establishes baseline understanding of the terms that the article will use. The introduction also enthuses the reader to keep on reading--there should be hints of wonderful concepts and code to come. You may use figures, tables and diagrams, but be sure to number these elements appropriately. (Viz Figure 1.)

######Figure 1. Overall architecture
![The figure caption](the-figure.png)

Use heading 6 for the figure caption and be sure to number the figure correctly. Then use the standard markdown syntax.

#Dive in
The following paragraphs should start exploring the outline from the introduction. This is the place to use ``inline code`` as well as larger listing blocks; as shown in Listing 1.

######Listing 1. The domain
```scala
	case class Name(first: String, last: String, middle: Option[String] = None)
	case class Address(line1: String, line2: String, ...)
	case class Addresses(addresses: Seq[Address])
	case class Customer(name: Name, addresses: Addresses)
```

Just like figure captions, use heading 5 for the listing caption; it would also be helpful if you indicated the syntax highlighting to be used for the code listing. (See Listing 2.)

######Listing 2. Incoming XML:
```xml
    <customer>
      <name first="Jan" last="Machacek" middle="Křtitel"/>
      <addresses>
        <address line1="" .../>
        <address line2="" .../>
      </addresses>
      ...
    </customer>
```

##NoSQL stores
Naturally, you will want to use nested headings to outline the structure of the OSJ, however, do not go below heading 3.

###The deepest heading
Heading 3 is the deepest preferred nesting level; if you find yourself using heading 4, you should consider restructuring your document. Remember that there is no top-level heading 1. This entire document is your article, therefore Introduction, Deep dive, ..., and Summary should all be styled as heading 1. (The only exception is heading 5 for the listing, figure and table captions.)

#Summary
Remember to close your article with a summary, re-iterating the most important points. It is a good idea to refer back to the introduction and make sure that you address all the main topics you inclded in the introduction here. The summary should leave the reader feeling *warm & fuzzy* with all the new knowledge he or she has just attained.

#References
I encourage you to use the ISO-960 style for references & citations. Remember in particular that the OSJ is a *printed* publication; and long URLs might be difficult for the readers to type in. Consider using URL shortening services.