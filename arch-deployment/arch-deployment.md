# Deployment on Arch Linux

_This article provides a quick tutorial for getting a simple application
running on Arch Linux. The aim is to give developers who haven't much
Unix experience a chance to learn how it's done, and the best way is to
learn-by-doing._

Engineers like to understand how things work. Some systems, like a bicycle for instance, are easy to understand because they don't try to hide things. You can understand how a bicycle works by watching someone ride one!

![A bicycle doesn't try to hide its workings](bicycle.png)

Understanding how things work is a pre-requisite to fixing problems with
them. An open system also enables the freedom for users to make custom
modifications, giving the system more flexibility and allowing them to
service the system more cost effectively. The opposite is a 'black box',
a system that obscures how it works, either intentionally, as with
closed-source proprietary software, or as a trade-off to optimise
performance, as with compiled binaries.

Staying with the bicycle example, I've recently had to take my bike to
get serviced, since the gears are sticking.  I recently had to take my
bike to be serviced, since the gears were sticking. I was quoted £15 to
have it fixed.  A similar fault occured in our car, but the garage
quoted £750 to fix that. I'm sure that the smaller quote reflects how
much easier it is to understand, access and adjust the internal workings
of a bike.

In the same way that free and open-source software has helped developers
understand how programs work, we should extend the same idea to our
system deployments to make things as transparent as possible for those
who have to support and maintain these systems (and very often, that's
us as well!). If we are trying to optimise _understanding_, we should be
building 'white boxes' rather than 'black boxes'.

This idea is echoed in _The Arch Way_, a set of guiding principles for Arch Linux.

> "Arch Linux developers and users believe that trying to hide the
  complexities of a system actually results in an even more complex
  system, and is therefore to be avoided... It has a streamlined set of
  succinctly commented, clean configuration files that are arranged for
  quick access and editing, with no cumbersome graphical configuration
  tools to hide possibilities from the user." - The Arch Way

As an example, we'll pick a Clojure application to install on Arch. One aspect of Clojure that conforms to Arch principles is the fact that Clojure programs can be deployed in readable source-code form, like scripts. This makes things easier to understand and configure, compared to the approach taken by pre-compiled languages which are delivered in binary form. The problem with binary artefacts, even where it is possible to locate the original corresponding source code, support and deployment staff have to go to extra lengths to do so.

Clojure is compiled rather than interpreted, and Clojure systems have
comparable performance to Java and C# systems. However, the difference
is that Clojure typically compiles source code 'just-in-time' at
runtime, rather than 'ahead-of-time'. So Clojure systems can provide the
'best of both worlds': transparent workings without sacrificing
performance.

Finally, I should point out there are easier, quicker and more
convenient ways of deploying Clojure applications, often automating many
of the steps in this tutorial. For example, it's possible to deploy
Clojure applications quickly on Google App Engine, AWS Beanstalk,
Heroku, not to mention the other Linux distributions that provide more
'pre-installed' services than Arch. Buf if you're prepared to accept a
longer learning curve at the beginning, the benefits of a better
understanding of the _system as a whole_ as a 'full-stack' developer
will accrue over many years to come.

So first we are going to start at the beginning and build up. I won't explain everything in detail, so be ready to use a search engine to find out more!

# Be secure!

Before we start we'll need a host to deploy onto. You may already have an account on a host that you can use, but let's assume you don't. You'll need some security keys to access the host, so we'll start with this.

Securing access to a host is achieved by only permitting access to those in possession of a secret key. If someone is able to make a copy of this key, they'll be able to access your host, so it's a good idea to encrypt the key with passphrase (a long password). Good security is a combination of something you have (in this case, your key) with something you know (in this case, your passphrase).

If you don't already have a key on your local machine, you can create one via the terminal, using ssh-keygen :-

##### Creating a key with ssh-keygen
```
local$ ssh-keygen
Generating public/private rsa key pair.
Enter file in which to save the key (/home/bob/.ssh/id_rsa):
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /home/bob/.ssh/id_rsa.
Your public key has been saved in /home/bob/.ssh/id_rsa.pub.
The key fingerprint is:
3c:44:66:07:ce:71:0e:17:97:ef:c6:48:e9:e3:93:47 osj@laptop
The key's randomart image is:
+--[ RSA 2048]----+
|        B.=...   |
|       * B ..    |
|        + .  o   |
|       o    o .  |
|        S  o +   |
|         .  + E  |
|           . =   |
|            + .  |
|             o   |
+-----------------+
```

Hit Enter at the first prompt, but make sure you type a passphrase to protect your key in case of theft.

# Adding the host

If you don't already have a host, it's easier than you think to get one. We'll use __Digital Ocean__, although there are many others. Go to https://www.digitalocean.com/ and create an account. Use the coupon ```VPSERS10``` and you'll get plenty of free credit.

Once your account has been successfully created and you are logged in, go to the SSH Keys tab.

Now you'll need the content of the public half of your key :-

##### Viewing the contents of your public key
```
local$ cat ~/.ssh/id_rsa.pub
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjYle0uCq8Xf4JuoCoDbWUDarY++86sHBr+yZkxEgKWkK9fOsTlvOGERlBP++TUqSuieelbheme9kK4wXXhFYWdLtZtCa/3u8hz5R1C99bFdllkyQi/6NO4pe293wCUZRSrd55ERKVO2Gk24RsUgcLKInzSyEmf17Tv3SIKfigw8Td36F2bQzKzStyxI4lrCIhbm+w96rwK5nPD2HJbBY0VPc3WV56taLi14Eto8BmByzYD/EoyXDgBDAWAkMTrFYLM6yOtIEjc3uqW+/oAeJJkjMAyK8wM3xq8ZDwZoV709uMuN4X6Cb49l5ZPy9TQfKErykp9kFgEcw1XgINtlqR bob@example.org
```

Copy and paste the text of your public key from the console into the web form, give the key any name you like, and click CREATE SSH KEY.

Now go to the droplets tab and click on Create Droplet. This takes you to a page containing a web form where you can select the size of a new virtual host. A 512MB/1 CPU instance with 20GB of disk will be fine.

Next choose your region. Then, under Select Image (ensure the Linux Distributions tab is selected), choose Arch Linux and select the 64-bit version. Finally, select the SSH key you added and submit the form to create the new virtual host. The resulting table will tell you its IP address.

# Adding the domain name

There are many domain registrars which can register a domain name for you, if you don't already have one. They all come with a DNS control panel where you can add an entry to register a host in your domain to the IP address associated with the host. Now is a good time to add an entry, since it may take a few hours to propagate across the Internet's DNS servers. In this article we'll use ```www.example.com``` as the hostname, replace this with your host-name in the instructions coming up.

Test that you can login into the new host :-

##### Logging in to your machine
```
local$ ssh www.example.com
The authenticity of host 'www.example.com (82.196.7.118)' can't be established.
ECDSA key fingerprint is 5d:8c:b2:e7:3b:b3:13:f4:3d:d4:db:c2:2c:5d:df:d1.
Are you sure you want to continue connecting (yes/no)?
```

When you confirm with yes, a _host-key_ is stored in the file ```.ssh/known_hosts```. If the remote host signature changes, for example, if the IP address changes (perhaps due to DNS poisoning), you'll be warned and asked to confirm again (this measure is to detect 'man in the middle' attacks). Type ```yes``` and you'll see a console prompt.

From now on, this console prompt indicates which machine which host to type commands into, and the # suffix indicates, by convention, that you are logged in as the root user.

# Adding system dependencies

With Arch, we _sync_ our system with on-line Arch repositories which contain trusted signed binary software packages. It's also possible to build from source, if you want to take the principle further, but it's easier to use the binary packages others have built. First, we must update the indexes on our machine so it knows which packages are available.

```
prod# pacman -Sy
```

Now we'll need to install Java, because Clojure applications run on it,
and git, to pull down our Clojure application source code. Installing
packages on Arch is very easy and fast.

```
prod# pacman -S jdk7-openjdk git
```

# Adding an application account

Running applications under the powerful root user is dangerous, so let's create an application account.

```
prod# useradd -mr osj
prod# chmod 755 /home/osj
```

# Adding the application launcher: Leiningen

Leiningen is the de-facto tool for launching Clojure applications from
source code. While it's possible to install Leiningen system-wide via an
Arch package, it's more flexible to install a version of Leiningen in
each application account.

```
prod# su - osj
osj$ mkdir bin
osj$ curl https://raw.github.com/technomancy/leiningen/stable/bin/lein -o bin/lein
osj$ chmod +x bin/lein
osj$ echo 'PATH=$HOME/bin:$PATH' >> .bashrc
osj$ exit
prod# su - osj
```

# Adding the application

Now we get to the application itself. We'll use git to get the source onto the host.

```
osj$ git clone https//github.com/juxt/juxt-accounting
```

Notice that if our deployment changes in anyway, we'll be able to ask git exactly what has changed, using ```git status``` or ```git diff``` for example. This is at least as good, if not better, than the facilities provided by package managers to detect file modification.

# Adding the application launcher launcher

Let's tell Arch to manage this application for us. Arch uses _systemd_,
which is gradually becoming the de-facto standard in Linux distributions
(with the notable exception of Ubuntu). Since Arch is less complicated
that other distros it's cheaper and quicker for Arch developers (and
users) to adopt the latest advances.

We just add a single configuration file, which we can create with ```vi```.

```
prod# vi /etc/systemd/system/osj-app.service
```

Hit ```i``` (for insert mode) and paste the following configuration.

##### The systemd configuration file for our example
```
[Unit]
Description=My example OSJ app

[Service]
Type=simple
ExecStart=/home/osj/bin/lein trampoline run
Restart=always
User=osj
WorkingDirectory=/home/osj/juxt-accounting

[Install]
WantedBy=multi-user.target
```

Hit ESC, and then enter ```:wq``` to save and quit.

To start the application, we use ```systemctl```. We'll also enable the application so that it re-starts automatically if the machine is rebooted.

```
prod# systemctl enable osj-app
prod# systemctl start osj-app
```

After a while the application should come up and a website will be available via port 8000, eg. http://www.example.com:8000. You can see what is going on during start-up with ```journalctl```.

```
prod# journalctl -u osj-app -f
```

# Adding a reverse proxy

Not many websites still show port numbers in their URLs, and many are moving to https. A reverse proxy will allow you to direct traffic from the default http port (80) to the 8000 port used by the application, which isn't allowed to open on port 80 as it's not running under the root account.

A reverse proxy can provide us with the following benefits and more :-

* Routing traffic from 80 to 8000
* Handling SSL so we don't have to code for it in our application
* Providing a content cache, to take load off our application
* Spoon-feeding slower clients (like mobile phones) to reduce the number of connections to our application
* Serving static resources efficiently

These benefits add up, and make it worth configuring a reverse proxy.

For learning purposes it's sufficient to get free SSL certificates from a site like www.startssl.com. Save your certificate files under ```/etc/nginx```. I've named them osj.pro.crt and osj.pro.key in the example below.

```
prod# pacman -S nginx
```

Edit the /etc/nginx/nginx.conf file and insert the following in the http section.

```
http {

    proxy_cache_path  /var/cache/nginx/www.example.com levels=1:2 keys_zone=www.example.com:8m max_size=1000m inactive=600m;
    proxy_temp_path /var/cache/nginx/tmp;

    server {
        listen       443;
        server_name  www.example.com;
        ssl                  on;
        ssl_certificate      osj.pro.crt;
        ssl_certificate_key  osj.pro.key;
        ssl_session_timeout  5m;
        ssl_protocols  SSLv2 SSLv3 TLSv1;
        ssl_ciphers  HIGH:!aNULL:!MD5;
        ssl_prefer_server_ciphers   on;

        root /home/osj/juxt-accounting/public;

        location @app {
            proxy_pass              http://localhost:8000;
            proxy_set_header        X-Real-IP $remote_addr;
            proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header        Host $http_host;
        }

        location ~* \.(js|css|png|jpg|jpeg|gif|ico)$ {
            try_files $uri @app;
            expires 1d;
        }

        location / {
            index index.html;
            try_files $uri @app;
        }

    }
```

Before we start nginx, we need to make the directory where it will cache resources.

```
prod# mkdir -p /var/cache/nginx/www.example.com
```

Now enable and start nginx so that Arch manages it.

```
prod# systemctl enable osj-app
prod# systemctl start osj-app
```

You'll now be able to access your application over HTTPS: ```https://www.example.com```.

# Conclusion

This is been a fairly long tutorial and well done for getting to the end. I hope you feel you have learned something along the way.

If you want to use this as the basis for a production deployment, I suggest you add a firewall, take a look at the iptables link at the end.

Thank you for reading. If you have any questions or comments, please feel free to write to me at malcolm@juxt.pro.

# Useful links

* [Arch Linux](https://www.archlinux.org/)
* [Arch iptables](https://wiki.archlinux.org/index.php/Iptables)
