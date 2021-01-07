from abc import ABC, abstractmethod
from typing import Any, Sequence


class EntityDoesNotExist(Exception):
    pass


class Event:
    def __init__(self, id: int, name: str, year: int, description: str) -> None:
        self.id = id
        self.name = name
        self.year = year
        self.description = description

    @classmethod
    def from_dict(cls, ev_dict: dict[str, Any]) -> 'Event':
        return Event(
            id=ev_dict['id'],
            name=ev_dict['name'],
            year=ev_dict['year'],
            description=ev_dict['description'],
        )

    def to_dict(self) -> dict[str, Any]:
        return {
            'id': self.id,
            'name': self.name,
            'year': self.year,
            'description': self.description,
        }


class EventRepo(ABC):
    @abstractmethod
    def list(self) -> Sequence[Event]:
        pass

    @abstractmethod
    def get(self, id: int) -> Event:
        pass

    @abstractmethod
    def save(self, event: Event) -> None:
        pass

    @abstractmethod
    def create(self, ev_dict: dict[str, Any]) -> Event:
        pass

    @abstractmethod
    def delete(self, id: int) -> None:
        pass
