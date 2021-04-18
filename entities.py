from django.db import models


class Event(models.Model):
    name = models.CharField(max_length=255)
    year = models.IntegerField()
    description = models.CharField(max_length=255)
