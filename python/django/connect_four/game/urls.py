from django.urls import path
from . import views

urlpatterns = [
    path('', views.index, name='index'),
    path('drop_disc/', views.drop_disc, name='drop_disc'),
]
