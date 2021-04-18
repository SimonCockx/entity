from event.models import EventRepo
from django.http import JsonResponse, Http404
from django.utils.decorators import method_decorator
from django.views import View
from django.views.decorators.csrf import csrf_exempt
from django.urls import path
import json


class ListEventView:
    def get(self, request):
        return JsonResponse(
            [
                {
                    "id": event.id,
                    "name": event.name,
                    "date": event.date,
                    "description": event.description,
                }
                for event in EventRepo.list()
            ],
            safe=False,
        )

    def post(self, request):
        body = json.loads(request.body)
        EventRepo.save(
            Event(
                id=body["id"],
                name=body["name"],
                date=body["date"],
                description=body["description"],
            )
        )
        return JsonResponse({"message": "success"}, status=204)

    @method_decorator(csrf_exempt)
    def dispatch(self, *args, **kwargs):
        return super.dispatch(*args, **kwargs)


def register_urls(urls):
    urls += [path("api/events/", ListEventViewas_view())]
