Query a time server via NTP and display the current time with local time zone
taken into account.

Features:

- Low level networking with socket and UDP
- Low level bit/byte manipulation
- Date time API

Yes, there is a package for NTP to abstract away the dirty details, and there
is also a convenient wrapper around the raw Data.Time, but hey, where is the
fun in that?

Status: Completed