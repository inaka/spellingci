
# Spelling CI
Spelling CI Server is an application for checking the spelling of Pull Requests and it is built on top of [sheldon](https://github.com/inaka/sheldon). `Spelling CI` provides:

- A way to check the spelling of Pull Requests with the option of configuring it using a configuration file
- Web interface built in 'Angular JS' where users can manage their repositories (Adding and removing the webhook)

You can create your own server or use the one managed by [Inaka](http://www.inaka.net) at [Spellingci](http://spellingci.inakalabs.com/).

**Note** Users should create a `config/app.config` file in order to run `spellingCI`, this config file should be implemented following the `config/app.config.template`

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/spellingci/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

## Use
You can use this source code in order to deploy your own `Spelling CI Server`, remember if you decide to deploy your own you must create the file `config/app.config` following the structure described on `config/app.config.template`.

The landing page looks like:

![Alt text](/assets/img/landing.png?raw=true "Landing Page")

User must log in using Github credentials, after that user will be redirected to a list with his/her public repositories.

![Alt text](/assets/img/list.png?raw=true "Repositories List")

`Spelling CI` stores your list of repositories in mnesia backend. If your recent repositories doesn't appear on the list you should click the "Sync" button in order to synchronize with github again.

Users should check the repositories they want to be webhooked by `Spelling CI`. Once it is checked, every time the repository receive a Pull Request `Spelling CI` will check the spelling regarding the configuration, (if no configuration file is found `Spelling CI` will check only `*.md` and `*.markdown` files by default).

# spellingci.json
`Spelling CI` allow us to configure your settings through a json file. this is the format:
```
{
  "extensions" : ["txt", "other"],
  "ignore_words" : ["word1", "word2],
  "ignore_patterns" : ["regex1", "regex2"],
  "ignore_blocks" : [{"open" : "open_regex", "close" : "close_regex"}]
}
```
`ingore_words`, `ignore_patterns` and `ignore_blocks` are directly related with [sheldon](https://github.com/inaka/sheldon) configuration. `extensions` are the extensions we want to check (remember, `*.md` and `*.markdown` by default if no `spellingci.json` file is present).

# Example
we have a repository linked to our Spelling CI webhook. Those are the files:

![Alt text](/assets/img/files.png?raw=true "Files")

The names are self explanatory :)

Lets try to do a Pull Request

![Alt text](/assets/img/pr1.png?raw=true "Pull Request 1")

Only `*.md` and `*.markdown` are checked, makes sense. But what if we want to check different extensions or even ignore the `*.md`? `SpellingCI` allow us to configure our settings through `spellingci.json` file. Lets take a look with a simple `spellingci.json`.
```
{
  "extensions" : ["txt", "md"]
}
```
Adding the `spellingci.json` file and push again.

![Alt text](/assets/img/pr2.png?raw=true "Pull Request 2")

`Spelling CI` is checking txt files now and it is not happy...

![Alt text](/assets/img/fail.png?raw=true "I am not happy")

Now the committer should make `Spelling CI` happy if he/she wants the contribution to be merged. In order to make it happy you should update your misspelled words and/or update the `spellingci.json` file, by updating it I don't mean using this config file...

```
{
  "extensions" : []
}
```

I mean to add some `ignore_words` or `ignore_blocks`, that depends your case. But finally we will achieve it!

![Alt text](/assets/img/passok.png?raw=true "I am happy now!!")
