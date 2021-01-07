import json
from typing import Any

from app.entities import Event


class EventEncoder(json.JSONEncoder):
    def default(self, o: Any) -> dict[str, str]:
        if isinstance(o, Event):
            return o.to_dict()
        return super().default(o)
