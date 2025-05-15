# Silicon Gallery

A command line utility used to watch list of photo albums published
by [Silicon Hill](https://www.siliconhill.cz/photogalleries)
and send them to a Discord channel.

## Implementation milestones
- read config from env/args
- fetch HTML
- parse HTML (using a library) and extract models
- File IO - check already published photo albums
- Discord API - send a message (HTTP endpoint call with custom flags)
- File IO - store that a new album was just posted
- repeat every n minutes

