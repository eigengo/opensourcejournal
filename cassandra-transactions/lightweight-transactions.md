Cassandra 2.0: Lightweight Transactions and How to Use Them
===========================================================

Cassandra is an open source database aimed at applications that require scalability and fault-tolerance. It is primarily used within large web applications like Netflix, eBay, and Spotify, and is being adopted by retail, financial and media organizations. Cassandra supports scale-out across commodity or cloud machines as well as replication of data across multiple data centres or groups of nodes for high availability.

The focus of Cassandra development in 2013 has been the introduction and improvement of CQL, the Cassandra Query Language.  CQL lowers the learning curve for new users and makes both new and experienced developers much more productive.

The big news for the release of Cassandra 2.0 in September was the addition of lightweight transactions to CQL.  Lightweight transactions allow developers to ensure that a sequence of operations can be performed without interference by other requests, similar to the serializable isolation level offered by relational databases (RDBMS).

What are lightweight transactions?
----------------------------------

First, some background.  In the context of distributed systems, we say that an architecture exhibits strong consistency when a read request will always return the most recently written value.

It is easy to see how we can achieve strong consistency in a master-based system, where reads and writes are routed to a single master. However, this also has the unfortunate implication that the system must be unavailable when the master fails until a new master can take over.  The typical RDBMS deployment is the trivial case of such a master-based system, consisting of a single large machine replicated to a slave for failover.

A fully distributed system like Cassandra can provide higher levels of availability for these scenarios when a hardware failure is encountered: all replicas can respond to read and write requests, so no single failure will affect the running of the application. In such a system, we can still achieve strong consistency by performing quorum-based operations: if we require writes to be synchronously replicated to a majority of replicas, and reads to also consult a majority, we can provide the best of both worlds, with high availability while still ensuring that readers always get the most recent updates. 

For many applications, even most, this approach to consistency is completely adequate. However, there are some situations where strong consistency is not enough. What if some operations have to perform in a sequence that must not be interrupted by others and must be performed one at a time? How can we make sure that any of these transactions that we do run concurrently will get the same results as if they really were processed independently? This is linearizable consistency, or in ACID terms, a serial isolation level.

For example, suppose that I have an application that allows users to register new accounts. Without linearizable consistency, I have no way to make sure I allow exactly one user to claim a given account: even if I check for existence before performing the insert in one thread, I can’t guarantee that no other request inserts it after the check but before my own insert.

Lightweight transactions allow Cassandra 2.0 to meet this requirement while also still providing the distributed architecture and fault-tolerance for hardware failures that would affect RDBMS-based applications in the same situation. 

How does it work?
-----------------

Cassandra implements lightweight transactions through Paxos. Paxos is a consensus protocol that allows a distributed system to agree on proposals based on a quorum-based algorithm, with no masters required and without the problems of two-phase commit. There are two phases to Paxos: prepare/promise, and propose/accept. 

Prepare/promise is at the core of the algorithm. Any node may propose a value and we call that node the leader. Any of the nodes involved in the cluster may attempt to act as leaders simultaneously, so this is not a dedicated “master” role. Following this, the leader picks a ballot and sends it to the participating replicas. If the ballot value is the highest that a replica has seen, it promises not to accept any proposals associated with any earlier ballot. Along with that promise, it includes the most recent proposal it has already received.

If a majority of the nodes across the cluster promise to accept the leader’s proposal, it may proceed to the actual proposal. However, if a majority of replicas included an earlier proposal with their promise, then that is the value the leader must propose. Conceptually, this means that if a new leader (L1) interrupts an earlier leader (L2), L1 must first finish L2’s proposal before proceeding with its own. This approach ensures that we can get our desired linearizable behaviour.

This use of Paxos gives us the ability to agree on exactly one proposal. After this proposal has been accepted, it will be returned to future leaders in the promise, and the new leader will have to re-propose it again. We need a way to “reset” the Paxos state for subsequent proposals. In our case, what we really want to do is move the accepted value into “normal” Cassandra storage. 

To do this, we add a third phase at the end after the algorithm has gone through propose/accept. This is termed commit/acknowledge, when the leader carries out the change and the accepting nodes respond.

There is another point to bear in mind. Because Cassandra exposes this functionality as a compare-and-set operation, we need to read the current value of the row to see if it matches the expected one. This therefore requires an additional step between prepare/promise and propose/accept. This is termed read/results, and covers the leader looking at the promises from the accepter nodes and sending that value out. The results phase covers the accepter nodes providing this information back to the leader.

By using this approach, Cassandra can provide the ability to make transactions work in a linear way at the cost of four round trips between nodes in the cluster. This can be a high cost, particularly if you have the rare case of an application that requires every operation to be linear in nature. However, for most applications, only a very small minority of operations require full linear transactions in order to work. The remainder of operations may be supported through eventual consistency and strong consistency methods.

Using lightweight transactions
------------------------------

In CQL, lightweight transactions can be used for both ``INSERT`` and ``UPDATE`` statements, using the new ``IF`` clause. Here’s an example of registering a new user, as in our initial example:

    INSERT INTO USERS (login, email, name, login_count)
    VALUES ('jbellis', 'jbellis@datastax.com', 'Jonathan Ellis', 1)
    IF NOT EXISTS;

``UPDATE`` also allows the ``IF`` clause.  For example, suppose I want to keep a transactional count of views to a page.  I could implement this by first reading the current value, then incrementing it:

    SELECT * FROM page_views WHERE page_id = 2384;

     page_id | views
    ---------+-------
	2384 |  9902

    UPDATE page_views SET views = 9903
    WHERE page_id = 2384
    IF views = 9902;

How is this different from relational transactions?
---------------------------------------------------

There are two primary reasons for calling this feature "lightweight" transactions.  First, it is based on optimistic concurrency control rather than a pessimistic, lock-based system.  This means there is no way for requests to deadlock your database.  Second, it is not interactive; there is no ``BEGIN/COMMIT`` sequence during which arbitrary statements may be processed.  Instead, the developer gives Cassandra both the desired changes and the conditions to validate before making them, as a single unit.

In Cassandra 2.0, lightweight transactions provide developers with the ability to ensure that a given sequence of operations is performed as a unit, without interference from other requests. Unlike RDBMSes or other master-based databases, Cassandra does this while also providing a bulletproof, single-point-of-failure-free approach.  Lightweight transactions are part of the expanding surface area where Cassandra is an option for replacing legacy databases.
