#Changing History Using Git

#Introduction
From entirely removing sensitive information to fixing a reset/amended shared commit, here are some advanced recipes for getting out of sticky situations - along with a review of the basics of changing history in git.

#Changing History

One of the great things about git is that you can change your version control history. Messed up a commit message or forgot to commit a file? Amend your commit. Made three commits where one would do? Reset the commits and create one new one with all of the changes. However it can be confusing to keep track of all the different commands and options available for cleaning up your history, so in this article we'll look at how to solve some common problems around rewriting history by using git. But first we need to know when not to change history . . .

#Local vs. Shared History

There are two types of history that you need to treat very differently in git - local history that is only currently stored on your machine, and shared history that you have pushed to a server or shared with other developers. Generally you should not change shared history. If you do, you'll cause problems for anyone else working with you on the project (we'll look at how to fix that in a bit). If you have a commit that you wish to undo but that has been shared with someone, use the `git revert` command. It doesn't actually change your history. It just introduces a new commit that is the opposite of the one you are reverting - getting the working directory back to the condition it would have been in if the initial commit had never been made. So, the next few commands are ones that you should almost always limit to local history.

#Fixing the Last Commit

Have you ever made a typo in a commit message? Or maybe you've used the `git commit -a` and forgotten that it only adds modified - not untracked - files? Either way, you often know immediately that you've made a mistake. Luckily if you notice this before you push, there is a really easy fix.

To fix a commit message, just type:

######Listing 1. Fixing a commit
    git commit --amend -m "The fixed commit message"

It will re-create your last commit, changing the commit message to the new one you created.

If you forgot to add one or more files to your last commit, just add them using `git add` and then type:

######Listing 2. Another way to amend a commit
    git commit --amend

It'll throw you into your default commit message editor (if it's vi, `:wq` will get you out). Then run a git status and you'll see that the files you added are now committed and if you do a git log you'll see they were just added to the last commit as if they'd always been a part of it.

#Further Back

`git commit --amend` is great if you only want to change the most recent commit, but what if you've created two or three local commits that you want to change? Simple. We can use the `git reset` command to undo them. There are three options for `git reset` - soft, mixed (the default) and hard.


#Turning a Few Commits into One

Let's say you want to merge your last two commits into a single commit. Easy. Just type

######Listing 3. git reset --soft
    git reset --soft HEAD~2

This gets rid of the last two commits in history (returning you to the state as of two commits before HEAD - the most recent commit on this branch). However, it keeps all of your changes and even keeps them all staged. If you check your `git status` you'll see all your changes from the last two commits are in the staging area. So to create the new commit to replace the old two we just have to type:

######Listing 4. Re-committing
    git commit -m "Put your new commit message here"

And it'll create the new commit you want with the new message and all of the changes from the two commits you removed.

#Changing the Grouping of Your Commits

Imagine that you created three html files and a css file for each one and then did two commits. You typed:

######Listing 5. Setup for the problem
    git add *.html
    git commit -m "Added html files"
    git add *.css
    git commit -m "Added styling for new html files"

So you decided to commit all of the html files together and then all of the styling for them. That might be the right strategy, but let's say you decided you would have been better to make three commits - each with it's own html and css file. Again, not a problem. This time you just type:

######Listing 6. git reset --mixed
    git reset HEAD~2

This time we're using the default "mixed" mode instead of --soft. (We could have typed `git reset --mixed HEAD~2`, but didn't need to as the mode defaults to --mixed). Now run a `git status` and you'll see that you have got all of the files from the last two commits, but instead of being in the staging area ready for committing, they are just in the working directory. That is perfect for this use case. Now you can just add the files you want for each of the three commits that you want to make and then make your three commits.

#Pretending This Afternoon Never Happened

Ever had one of those afternoons? You're working hard, you make a number of commits and at the end of the afternoon you realize that you've been going in the wrong direction and completely wasted your time and those commits are of no value at all? If you want to completely get rid of (say) the last 5 commits you made, type:

######Listing 7. git reset --hard
    git reset --hard HEAD~5

This will delete the last five commits and also remove all of those changes from the working directory, so it's as if you didn't make any of the changes at all. Be really sure before you do this, but sometimes it's the best possible choice.


#Removing a Sensitive File from the Repository

How many times has someone new to your team inadvertently checked in a password file or a file containing an API secret key? Removing the file from the current state of the repository using a `git rm` just isn't enough. You need to completely remove the file from the history of the repo so that the sensitive information isn't available to everyone with the ability to clone your repository and search it's history. This is where `git filter-branch` comes in.

Let's start with some basic clean up. As soon as something has been pushed to a repo you should assume it has been compromised, so start by changing the password and then add the file to the .gitignore (which should be committed as part of the repo) so it won't get added again.

Now you want to use the filter-branch command to purge the file from the repo's history.

######Listing 8. git filter-branch
    git filter-branch --force --index-filter \
    'git rm --cached --ignore-unmatch <filepath>' \
    --prune-empty --tag-name-filter cat -- --all

Replace <filepath> with the full path of the file that you want to remove (e.g. /misc/passwords.txt). This command will go through the history of every branch and tag changing any commit that included the password file and any commits after them.

Now to share this, we've changed history, so we're going to have to force this to upload to our shared repo. Assuming a standard configuration with your main remote repo called "origin" you'd run:

######Listing 9. Forcing a push
    git push origin master --force

If this affected any other branches or tags, you're also going to have to push those using the --force option.

You'll still have a copy of all of the affected files and commits in your local repo. To clean things up you can either blow away your repo and clone it again, or you can clear the reflog and force garbage collection by running:

######Listing 10. Forcing git garbage collection
    git reflog expire --expire=now --all
    git gc --aggressive --prune=now

Note that the commits containing the password file do still exist on the remote server and could theoretically be accessed via their SHA1 hash. If you *really* need to clear that, you'd have to re-create the remote repository.

#Fixing Changed Shared History

Whether you've just run a filter-branch or someone else has pushed, reset and then force-pushed a commit, sometimes your repository is going to get into a state that will mess with any collaborators you have. If someone changes history and then someone else who had the old version of history merges the new history they're doing to get some fairly tricky merge conflicts.

The trick is to get your team to rebase before merging upstream changes. If they `git rebase` it'll take the changed history and replay local changes on top of the new history. Sometimes a straightforward rebase will work if the changes are the same in their history and in the rewritten shared history, but if the changes are different, you get a "hard" situation where the rebase will try to replay similar changes from the old shared history. To be safe, it is best to `git rebase --onto` instead. Let's say you have made some local commits on a branch called _feature_ and want to keep those but on top of the new shared history. 

Let's assume we're working on a master branch. Start by getting the new shared history using a fetch, then rebase --onto:

######Listing 11. Rebasing onto changed shared history
    git fetch origin/master
    git rebase --onto origin/master master feature
    git reset --hard origin/master

This will take the new history from origin/master and replay your local _feature_ changes on top of it, discarding any conflicting or out-of-date _master_ branch commits and solving the problem. It also resets the working branch _master_ to match origin/master so you now have the same shared history as everyone else. When you're done with the _feature_ you can then `git merge feature` onto your master branch and successfully push to the remote server.

#Summary

There are even more ways you can play with history in git. If you're interested in another approach, google "interactive rebase git" for another really cool way to squash, reorder and even completely eliminate local commits. And if you get into trouble, google for information on the "git reflog" which allows you to get back commits that you have reset or amended. I hope the hints and tips above have been of some use. If you'd like to get regular hints and tips on better ways to use git and github, you can sign up at http://gitandgithubrecipes.com for regular recipes to keep improving your usage of git!
