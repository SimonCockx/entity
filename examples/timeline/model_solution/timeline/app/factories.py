from app.entities import EventRepo
from app.models import DjangoEventRepo
from app.views import EventView


class EventRepoFactory:
    @staticmethod
    def create() -> EventRepo:
        return DjangoEventRepo()


class EventViewFactory:
    @staticmethod
    def create() -> EventView:
        return EventView(EventRepoFactory.create())
