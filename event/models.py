from django.db import models


class Event(models.Model):
    name = models.CharField(max_length=255)
    date = models.DateField()
    description = models.CharField(max_length=255)


class EventRepo:
    @staticmethod
    def list():
        return Event.objects.all()

    @staticmethod
    def get(id):
        return Event.objects.get(id)

    @staticmethod
    def save(event):
        event.save()

    @staticmethod
    def create(name, date, description):
        return Event.objects.create(name=name, date=date, description=description)
