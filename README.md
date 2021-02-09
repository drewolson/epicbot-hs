# Epicbot

Epicbot is a slack bot written in Haskell. It allows you to search for cards
from the [Epic Card Game](https://www.epiccardgame.com/) and display the results
in your slack channel.

To search for a card, simply type some or all of its name. Epicbot will perform
a full text search to find the correct card.

```text
/epic <search term>
```

You can also generate an example random dark draft draw using the `draft`
command.

```text
/epic draft
```

## Deploying

```text
stack --docker build
```

This builds the `epicbot-exe` executable on the docker container referenced in
`stack.yaml`. If you're deploying to an environment different than `debian`,
feel free to change this image.

To find the location of the executable you build, run the following:

```text
stack --docker exec -- which epicbot-exe
```

Now, run the executable.

```bash
EPICBOT_ONLINE=1 EPICBOT_SLACK_SIGNING_SECRET=<your slack signing secret> /path/to/epicbot-exe
```

Once you have your bot running, you'll want to follow the guide for creating a
[slash command](https://api.slack.com/interactivity/slash-commands) to use the
bot. We recommend using `/epic` as your slash command.
