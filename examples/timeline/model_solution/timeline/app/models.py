from collections import Sequence
from typing import Any

from django.db import models

from app.entities import EventRepo, Event, EntityDoesNotExist

MAX_NAME_LENGTH = 100


class EventORM(models.Model):
    name = models.CharField(max_length=MAX_NAME_LENGTH)
    year = models.IntegerField()
    description = models.TextField()

    @classmethod
    def from_entity(cls, event: Event) -> 'EventORM':
        return EventORM(event.id, event.name, event.year, event.description)

    def to_entity(self) -> Event:
        return Event(self.id, self.name, self.year, self.description)


class DjangoEventRepo(EventRepo):
    def list(self) -> list[Event]:
        return [q.to_entity() for q in EventORM.objects.all()]

    def get(self, id: int) -> Event:
        try:
            return EventORM.objects.get(id=id).to_entity()
        except EventORM.DoesNotExist:
            raise EntityDoesNotExist()

    def save(self, event: Event) -> None:
        EventORM.from_entity(event).save()

    def create(self, ev_dict: dict[str, Any]) -> Event:
        q = EventORM.objects.create(**ev_dict)
        return q.to_entity()

    def delete(self, id: int) -> None:
        EventORM.objects.filter(id=id).delete()
