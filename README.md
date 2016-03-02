# JaTesté

A new language that makes it easy to test the functionality of your code. It's not quite done yet... but it's gonna be pretty cool when it is. 

Workflow overview for contributing to JaTesté
---------------------------------------------
We don't want to break any existing code so we'll only merge changes that adhere to the following workflow. 

1. Never merge to master!
2. Create your own fork of this repo and push any code you write to that branch. You can make a fork by clicking the "Fork" button on the top right of this page. To set up the your fork on your local machine so you can pull from here and push to your fork do the following from within your JaTeste directory: 

  ```git clone git@github.com:jaredweiss/JaTeste.git``` (this will clone JaTeste's master source into a directory called JaTeste. Odds are you've already done this part)
  
  ```git remote add me git@github.com:<myGithub>/JaTeste.git``` This part adds your own fork to the local git environment
  
  Confirm you've set up two remotes by running ```git remote -v```. You should see something along the lines of:
  
  ```
  origin	git@github.com:jaredweiss/JaTeste.git (fetch)
  origin	git@github.com:jaredweiss/JaTeste.git (push)
  me	git@github.com:<myGithub>/JaTeste.git (fetch)
  me	git@github.com:<myGithub>/JaTeste.git (push)
  ```
  
  You will be using the "me" remote to store all of your code. You'll also create pull requests from your remote. 

3. Push to your fork!

  Use the code `git push me master` instead of `git push origin master` in order to push your code to your own fork. This will prevent annoying merge conflicts and a potentially broken master. You should also regularly use `git pull origin master` to keep your local code synced with the main master. 
  
4. Use pull requests! 

  When you want to bring your code into the master repo, go to your fork and click on "New Pull Request". From there you can open a pull request and write about what the changes you made do. That way we can all look it over and make sure it works. This keeps multiple sets of eyes on the code before it goes into master! For more info on pull requests check out this [great guide](https://help.github.com/articles/using-pull-requests/) by github.

5. Checking out other people's code

  You may find that you need to pull code from someone else's branch in order to review it, or in order to work on your own code that builds upon things not yet merged into the mainline branch. To pull another branch, the first thing you need to do is add the user's fork as a remote (i.e. `git add remote <remote-name> git@github.com:<otherUser>/JaTeste.git`). Then, you just need to fetch from that remote (`git fetch <remote-name>`) and locally check out the branch you want to work on (`git checkout -b myLocalBranch <remote-name>/<remote-branch>`). For safety reasons run `git branch --unset-upstream myLocalBranch`, that way you don't accidentally git push to someone else's branch (you can always do a manual push anyway using `git push <remote-name> myLocalBranch` or `git push <remote-name> myLocalBranch:<remote-branch>` if the branch names differ). 
  
  The `-b` flag in the previous command tells git to create a new local branch with the name `myLocalBranch` from the branch `remote-branch` on the `remote-name` fork. This is now a branch that exists locally on your machine and any commits you make here can be pushed to your own github fork's branch by using the command `git push me myLocalBranch`. If you ever need to update the branch with new commits from it's original remote (remote-name/remote-branch), just run `git pull <remote-name> <remote-branch>`. 
  
  To view all available remote branches run `git fetch` and then `git remote -av`. Branches beginning with `remotes/` are the remote branches you haven't checked out locally. 
