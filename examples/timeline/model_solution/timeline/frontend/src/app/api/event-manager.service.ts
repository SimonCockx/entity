import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Event, InitialEvent, JSONEvent } from './event';
import { map } from 'rxjs/operators';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class EventManagerService {
  private readonly url = '/api/events/';

  private entities: Map<number, Event>;

  public constructor(private http: HttpClient) {
    this.entities = new Map();
  }

  public get(id: number): Event | null {
    return this.entities.get(id) || null;
  }
  public getAll(): IterableIterator<Event> {
    return this.entities.values();
  }
  private add(event: Event): void {
    this.entities.set(event.id, event);
  }
  public delete(event: Event): Observable<void> {
    this.entities.delete(event.id);
    return this.http.delete(`${this.url}${event.id}/`).pipe(
      map((response) => {})
    );
  }

  public load(): Observable<Event[]> {
    return this.http.get<JSONEvent[]>(this.url).pipe(
      map((response) => {
        return response.map(jsonEvent => this._create(jsonEvent));
      })
    );
  }
  public create(initEvent: InitialEvent): Observable<Event> {
    return this.http.put<JSONEvent>(this.url, initEvent.toJSON()).pipe(
      map((response) => this._create(response))
    );
  }
  private _create(jsonEvent: JSONEvent): Event {
    const event = new Event(jsonEvent.id, jsonEvent.name, jsonEvent.year, jsonEvent.description);
    (event as any)._isSynced = true;

    this.add(event);
    return event;
  }
}
