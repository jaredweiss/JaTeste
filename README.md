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

  When you want to bring your code into the master repo, go to your fork and click on "New Pull Request". From there you can open a pull request and write about what the changes you made do. That way we can all look it over and make sure it works. This keeps multiple sets of eyes on the code before it goes into master!
