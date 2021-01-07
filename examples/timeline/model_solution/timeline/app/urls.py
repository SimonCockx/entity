from django.urls import path

from app.factories import EventViewFactory


event_view = EventViewFactory.create()
urlpatterns = [
    path('events/', event_view.as_list_view()),
    path('events/<int:id>/', event_view.as_single_view()),
]
