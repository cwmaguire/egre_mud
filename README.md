EGRE_MUD - A MUD engine built on EGRE, the Erlang Graph Rules Engine

EGRE_MUD is the go-between for EGRE, the graph rules engine, and any
MUD that uses the rules engine.

```mermaid
flowchart TD
    em1["egre_mud_1"] --> em["egre_mud"]
    etd["egre_tower_defence"] --> ea["egre_arcade"]
    em & ea --> e["egre"]
```

EGRE_MUD handles:
- player connections through websockets
- loading rules from a MUD (e.g. EGRE_MUD_1)
- passing player input to the graph (i.e. EGRE)
- passing MUD output back to players

EGRE_MUD was split off of GERLSHMUD and is just the MUD engine. It will
use EGRE for the rules engine and will not implement anything being a
test MUD.

How it Works (short version):
All MUD elements (rooms, characters, attributes, weapons, spells) are
rule engine processes.
Processes live in a graph.
Messages propagate through the graph.
Messages succeed or fail.
Messages can be: generated, resent, broadcast, failed, passed, subscribed to.
