import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, UrlTree } from '@angular/router';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { EventManagerService } from './event-manager.service';

@Injectable({
  providedIn: 'root'
})
export class LoadEventsGuard implements CanActivate {
  public constructor(private manager: EventManagerService) { }

  public canActivate(
    next: ActivatedRouteSnapshot,
    state: RouterStateSnapshot): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {

    return this.manager.load().pipe(map(() => true));
  }

}
