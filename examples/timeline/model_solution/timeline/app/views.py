import json

from django.http import JsonResponse, Http404
from django.utils.decorators import method_decorator
from django.views import View
from django.views.decorators.csrf import csrf_exempt

from app.entities import EventRepo, EntityDoesNotExist, Event


class EventView:
    def __init__(self, repo: EventRepo, **kwargs) -> None:
        super().__init__(**kwargs)
        self.repo = repo

    def get_by_id(self, request, id: int):
        try:
            return JsonResponse(self.repo.get(id).to_dict(), safe=False)
        except EntityDoesNotExist:
            raise Http404()

    def get_list(self, request):
        return JsonResponse([event.to_dict() for event in self.repo.list()], safe=False)

    def post(self, request):
        self.repo.save(Event.from_dict(json.loads(request.body)))
        return JsonResponse({
            'message': 'Success',
            'status': 204,
        }, status=204)

    def put(self, request):
        new_event = self.repo.create(json.loads(request.body))
        return JsonResponse(new_event.to_dict(), safe=False, status=201)

    def delete(self, request, id: int):
        self.repo.delete(id)
        return JsonResponse({
            'message': 'Success',
            'status': 204,
        }, status=204)

    def as_single_view(self):
        return SingleEventView.as_view(event_view=self)

    def as_list_view(self):
        return ListEventView.as_view(event_view=self)


class SingleEventView(View):
    event_view = None

    def get(self, request, id: int):
        return self.event_view.get_by_id(request, id)

    def delete(self, request, id: int):
        return self.event_view.delete(request, id)

    @method_decorator(csrf_exempt)
    def dispatch(self, *args, **kwargs):
        return super().dispatch(*args, **kwargs)


class ListEventView(View):
    event_view = None

    def get(self, request):
        return self.event_view.get_list(request)

    def post(self, request):
        return self.event_view.post(request)

    def put(self, request):
        return self.event_view.put(request)

    @method_decorator(csrf_exempt)
    def dispatch(self, *args, **kwargs):
        return super().dispatch(*args, **kwargs)
