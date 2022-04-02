## Version 1.1

### Changes

- XC milestones to trigger alerts are now every 15km (winter) and 25km (summer)
- Removed '(No OGN Reg)' pilots from XC distance alerts. Only pilots registered with a public 'registration' name on ddb.glidernet.org will trigger XC alerts. This will reduce the number of XC alerts and mean that the service will only alert you that somebody is on a good XC if it can tell you who they are.
- Pilots will no longer trigger XC distance alerts unless their FLARM ID is set to visible
- Added minimum 300' AGL check to pilots triggering XC alerts to make sure they're actually airborne and not driving home
- Messages will no longer be sent in the hours of darkness, even if a device is detected near a takeoff site
- Switched map links from glidernet to Glide and Seek, which has nicer maps, shows glider tracks if you click on a glider, and allows to link directly to an aircraft by its ID, so clicking an XC alert link will take you to a pilot's current position, even if they've moved since the alert was sent. Note that links to pilots on XC will display at zoom level 9 and  need to be manually zoomed in after clicking the link. A query has been raised with gliderradar devs for possibility of increasing zoom on direct links to glider ID's
- Dunstable club sites have been moved to a new Telegram group 'Dunstable' to separate them from the South East group
- *All groups* message broadcast function added to be used for update news etc.
- Special celebratory message text when flights cross 100km boundaries.
- Fixed local time issue in summary alerts so that messages report BST properly
- Fixed altitude error on XC alerts where quoted altitude was being taken from previous set of pings, not live position

### Notes

- There has been one case of alerts sending duplicate XC messages, because two pilots were flying XC together and one had borrowed a FLARM device from the other. This would also happen if a single pilot flew with two FLARM devices, or a FLARM device and an OGN mobile phone tracker. Left unchanged for now but if it turns out to be a frequent issue, gaggles may be grouped into a single alert message for version 1.2

### Version 1.0

- Initial working release