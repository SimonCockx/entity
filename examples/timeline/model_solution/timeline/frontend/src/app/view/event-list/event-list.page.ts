import { Component } from '@angular/core';
import { finalize } from 'rxjs/operators';
import { EventManagerService } from 'src/app/api/event-manager.service';
import { Event, InitialEvent } from '../../api/event';

@Component({
  selector: 'app-event-list',
  templateUrl: './event-list.page.html',
  styleUrls: ['./event-list.page.scss'],
})
export class EventListPage {
  public events: Event[];

  public newEvent: InitialEvent;
  public isCreating = false;

  public constructor(private eventManager: EventManagerService) {
    this.events = Array.from(eventManager.getAll());
    this.newEvent = new InitialEvent('', 0, '');
  }

  public addEvent(): void {
    this.isCreating = true;
    this.eventManager
      .create(this.newEvent)
      .pipe(
        finalize(() => {
          this.isCreating = false;
        })
      )
      .subscribe(
        (event) => {
          this.events.push(event);
          this.newEvent = new InitialEvent('', 0, '');
        },
        (err) => {
          alert('Something went wrong :o.\n' + err.message);
        },
      );
  }

  public deleteEvent(event: Event): void {
    this.events.splice(this.events.indexOf(event), 1);
    this.eventManager
      .delete(event)
      .subscribe(
        () => {},
        (err) => {
          alert('Something went wrong :o.\n' + err.message);
        },
      );
  }
}
