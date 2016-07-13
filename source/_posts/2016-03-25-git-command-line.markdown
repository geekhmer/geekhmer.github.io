---
layout: post
title: "Git Command Line"
date: 2016-03-25 15:01
comments: true
categories: [Other]
keywords: Git Command Line
---

<p>
  <img src="/images/logo_git.png" alt="Git Command Line" />
</p>

<p>
  <a href="http://geekhmer.github.io/blog/2015/02/09/install-git-on-linux-slash-ubuntu/" target="_blank">How to install GIT on Linux/Ubuntu</a> -- Today, Source Version Control has gained popularity in the management of source code. Therefore, the software engineer needs to know how to use and manipulate GIT. The following common GIT command line will help you manipulate GIT:
</p>

<p>
  <strong>Set up Git Configuration</strong>
</p>

{% codeblock lang:ruby %}
git config --global user.email "your_email@domain_name.com" /* Setup email is used to commit */

git config --global user.name "your user name" /* Setup username is used to commit */

git config --global core.editor "vi" /* Choose editor used by GIT */

git config --global color.ui true /* Setup color ui for command line */

git config --list /* See Git configuration */
{% endcodeblock %}

<p>
  <strong>To Initialise a Local Repository</strong>
</p>

{% codeblock lang:ruby %}
git init 
{% endcodeblock %}

<p>
  <strong>Add a File to the Repository</strong>
</p>

{% codeblock lang:ruby %}
git init 
{% endcodeblock %}

<p>
  <strong>Commit the Change to Git</strong>
</p>

{% codeblock lang:ruby %}
git commit -m "message" 
{% endcodeblock %}

<p>
  <strong>See the Commits</strong>
</p>

{% codeblock lang:ruby %}
git log
{% endcodeblock %}

<p>
  <strong>Basic Commands</strong>
</p>

{% codeblock lang:ruby %}
git status  /*  The command 'git status' tells which files are not added or committed from Working to Staging to Repository */

git commit -m "message" /*  Commits and changes to all files that are in Staging into Repo  */

git diff /*  Show changes between Working and Local Repo, no file supplied shows all files  */

git diff --staged /*  Shows changes between Staged and Local Repo  */

git rm file.txt /*  Will remove file from working then git commit -m "" to also remove from Repo */

git rm --cached file.txt /* Leaves copy of file in Working but removes from Staging and Repo */

git mv /* Rename or move files - then git commit -m "" to move to Repo */

git commit -am "text goes here" /* Adds all files straight to Repo from Staging if they have changes - meaning they skip git add */

git checkout -- file.txt /* Restore Repo file to Working Directory using current branch  */

git reset --soft HEAD^ /* Restore repo file to staging */

git reset HEAD file.txt /*  Move a Stage file out of Stage back to Working */

git commit --amend -m "message" file.txt /* Change last commit to Repo (only last one can change) */
{% endcodeblock %}

<p>
  <strong>Resetting & Reverting</strong>
</p>

{% codeblock lang:ruby %}
/* Reverting --soft --mixed --hard will go back to previous commits* /

git log /* Gets the sha1s so you can see the coomits where you want revert  back to */

git reset --soft sha /* Changes Repo but not Staging or Working */

git reset --mixed sha /* Changes Repo and Staging but not Working */

git reset --hard sha /* Changes all 3 Tiers */

git clean -f /* Remove untracked files from Working  */
{% endcodeblock %}

<p>
  <strong>Ignore File</strong>
</p>

{% codeblock lang:ruby %}
.gitignore /* Ignores files to track in Working / track the .gitignore file */

Global Ignore /* Create in home folder  */ 
.gitignore_global
.DS_Store
.Trashes
.Spotlight_V100
/* Add in  */

git config --global core.excludesfile ~/.gitignore_global /* Add to gitconfig */
{% endcodeblock %}

<p>
  <strong>Stop Tracking Changes</strong>
</p>

{% codeblock lang:ruby %}
git rm --cached file.txt /* Leaves copy in Repo and Working */
{% endcodeblock %}

<p>
  <strong>Commit Log</strong>
</p>

{% codeblock lang:ruby %}
git ls-tree HEAD
git ls-tree master
git log --oneline
git log --author="Bunlong"
git log --grep="temp"
{% endcodeblock %}

<p>
  <strong>Show Commit</strong>
</p>

{% codeblock lang:ruby %}
git show dc094cb /*  show SHA1 */
{% endcodeblock %}

<p>
  <strong>Commands on Branch</strong>
</p>

{% codeblock lang:ruby %}
git branch /* Show local branches * is the one we are on */

git branch -r /* Shows remote branches */

git branch -a /* Shows local and remote */

git branch newbranch /* Creates a new branch */

git checkout newbranch /* Switch to new branch */

git checkout -b oldbranch /* Creates and switches to new branch  */

git push origin newbranch /* Push new branch to remote */

/* Diff in Branches */

git diff master..otherbranch /* Shows diff */

git diff --color-words master..otherbranch /*  Shows diff in color */

git branch --merged /* Shows any merged branches */

/* Rename Branch */

git branch -m oldname newname

/* Delete  Branch */

git branch -d nameofbranch

/* Merge Branch  */

git merge branchname /* Be on the receiver branch to merge the other branch */

/* Merge Conflicts between the same file on 2 branches are marked in HEAD and other branch */

git merge --abort /*  Abort basically cancels the merge */
{% endcodeblock %}

<p>
  <strong>Manually Fix Files and Commit - The Stash</strong>
</p>

{% codeblock lang:ruby %}
git stash save "message"

git stash list /* Shows whats in stash */

git stash show -p stash@{0} /* Show the diff in the stash */

git stash pop stash@{0} /* Restores the stash deletes the tash */

git stash apply stash@{0} /* Restores the stash and keeps the stash */

git stash clear /* Removes all stash */

git stash drop stash@{0}
{% endcodeblock %}

<p>
  <strong>Remotes Commands</strong>
</p>

{% codeblock lang:ruby %}
git remote add origin https://github.com/bunlong/test.git /* Origin can be named whateve followed by the remote */

git remote /* To show all remotes */

git remote show origin /* To see remote URL*/

git remote remove origin /* To remove remote */

git remote rm origin /* To remove remote */
{% endcodeblock %}

<p>
  <strong>Clone project. Push from local to Remote</strong>
</p>

{% codeblock lang:ruby %}
/* Cloning a GitHub Repo - create and get the URL of a new repository from GitHub, then clone that to your local repo, example below uses local repo named 'nameoffolder' */

git clone https://github.com/bunlong/test.git nameoffolder

git push -u origin master /* Push to remote(origin) and branch(master) */

/* Push to Remote from Local - more - since when we pushed the local to remote we used -u parameter then the remote branch is tracked to the local branch and we just need to use... */

git push

git push origin newbranch /* Push a branch to a remote */
{% endcodeblock %}

<p>
  <strong>Fetch Changes from a Cloned Repository</strong>
</p>

{% codeblock lang:ruby %}
git fetch origin /* Pulls down latest committs from remote origin/master not origin, also pull down any branches pushed to Repo Fetch before you work Fetch before you pull Fetch often */
{% endcodeblock %}

<p>
  <strong>Merge with origin/master</strong>
</p>

{% codeblock lang:ruby %}
git merge origin/master
{% endcodeblock %}

<p>
  <strong>Fetch + Merge data ==> Pull</strong>
</p>

{% codeblock lang:ruby %}
git merge origin/master
{% endcodeblock %}

<p>
  <strong>Get Remote Branch</strong>
</p>

{% codeblock lang:ruby %}
git branch branch_name origin/branch_name /* This will bring the remote branch to local and track with the remote */
{% endcodeblock %}

<p>
  <strong>Delete Branch</strong>
</p>

{% codeblock lang:ruby %}
git branch -d branch_name
{% endcodeblock %}

<p>
  <strong>Checkout and Switch Branch and Track to Remote</strong>
</p>

{% codeblock lang:ruby %}
git checkout -b nontracking origin/nontracking
{% endcodeblock %}

<p>
  <strong>Remove Remote Branch</strong>
</p>

{% codeblock lang:ruby %}
git push origin --delete branch
{% endcodeblock %}

<p>
  <strong>Undoing Changes</strong>
</p>

{% codeblock lang:ruby %}
git checkout path-to-file /* Restores a file before it is staged */

git reset HEAD path-to-file /* If it is staged - restores a file from last commit and then git checkout path-to-file */

git checkout HEAD^ path-to-file /* If is staged and committed - restores from last commit */

git reset --hard HEAD^ /* Restore prior commit */
{% endcodeblock %}

<p>
  <strong>Tag</strong>
</p>

{% codeblock lang:ruby %}
git tag -a v1.0.0 -m "message" /* Tagging a commit with a version number*/

git push --tags /* Pushes tag info to master remote */

/* You can checkout a commit and add a tag to that commit by checking out its SHA */

git checkout f1f4a3d /* Checking out a commit - see the commit SHAS by git log */
{% endcodeblock %}
