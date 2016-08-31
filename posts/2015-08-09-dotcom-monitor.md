---
title: dotcom-monitor review
author: tomberek
toc: yes
tags: devops
---

# Introduction

I enjoy collecting tools of the trade and finding useful ones.
That is why I place often used ones on a quick to access [page](/index.html).
While trying to raise awareness for [dotcom-tools](https://www.dotcom-tools.com/), they came across my homepage they saw a collection of similar tools and asked for consideration of their page.

# Ping Test

The provided link was to their [ping test](https://www.dotcom-tools.com/ping-test.aspx).
Initial reaction to the page is that it was a bit bloated.
Almost anyone who would care to run a ping test probably doesn't care about anything below the "fold".

## ARE YOU HUMAN?
Next up, the "ARE YOU HUMAN" check was confusing.
Perhaps I am just slow. I saw "five-four" and typed in: `54`.
Wrong. `5-4`. Wrong. Refresh. Now I see "4+3".
Oh, it wants me to do math. `7`. Success!
Wow, I'd be done with a manual ping check by now.

<amp-img src="/images/2015-08-08/ping-test.png" alt="Ping Test" width="671px" height="370px" sizes="(min-width:800px) 60vw, 90vw"/>

Initial entry requires a mouse. Suggest using the `autofocus` [HTML5 attribute](http://www.w3schools.com/tags/att_input_autofocus.asp). Pressing `ENTER` after entering the domain does nothing. Hitting tab after entering an IP or domain goes to:

1) Don't know.
2) ARE YOU HUMAN input. Pressing `ENTER` here does nothing.
3) The "free no obligation 30 day trial" button a full screen down on the page.
4) Can't tell.
5) Other links at the bottom of the page.

Suggest a better flow. First off, the CAPTCHA is probably not needed and a waste of time.
The very first [OCR online](http://www.free-ocr.com/) I tried was able to read the text perfectly.
Perhaps try [reCAPTCHA](https://www.google.com/recaptcha/intro/index.html) or something similar. This also seems like premature optimization. Has there been any abuse yet? Why can't simple rate-limiting be used.

## The test

Now we're in business. There is a popup asking us to automate pings as a way to perform health checks. This only shows up on the first use of the tool.
Nevertheless, this seems a bit agressive. Simply provide the service and an unobtrusive way to learn about and sign up for the service.
This just gets frustrating and will push people away from the site.

<amp-img src="/images/2015-08-08/popup.png" alt="Ping Test" width="702px" height="596px" sizes="(min-width:800px) 60vw, 90vw"/>

After hitting "SKIP" we can see the results. Good distribution of locations, though very little detail on each one.
Error descriptions are generic, though usable. Some users may want to actually click on the test and see the raw output. They may want to know how many packets were lost, what IP DNS resolved to, source IP, etc.

## Overall

The site seems useful to check for connectivity to a server from around the world.
Nice, but I wonder what really someone would do if 3 out of 23 failed.
How does that provide any more information than a single sourced ping?
What actions would one take?
If the entire globe was successful, but southeast Asia had no connectivity, I don't think my efforts to debug or resolve the issue would have any impact.
Plenty of other people are already working on the problem. 

The scrollbar is slugish in its attempt to be "fluid". It seems to use a jquery custom scrollbar. Make it native; faster, responsive, less opportunity for odd errors.

# Website Speed Test

Next I looked at the [website speed test](https://www.dotcom-tools.com/website-speed-test.aspx).
This was a lot more fun. It had the same UI annoyances as the Ping Test, but the results seemed much more useful. You quickly get load times from around the world and some basic metrics. There also seems to be a caching effect for DNS; so the first request may be quite a bit different than subsequent effects.

<amp-img src="/images/2015-08-08/website-speed.png" alt="Ping Test" width="870px" height="386px" sizes="(min-width:800px) 60vw, 90vw"/>

The results seems useful, but as a competitor, I have found [www.webpagetest.org](https://www.webpagetest.org) to be more configurable, though it only tests from a single location. Both made me notice that I had significant SSL Handshake time. I use a 4096 bit RSA key; probably overkill. Sadly StartSSL doesn't provide ECDSA certs. Please contact me if anyone knows where one can get free signed ECDSA certificates.

# DNS Tool

Clicked on the "TRY IT NOW" button at [https://www.dotcom-monitor.com/server-monitor/dns-monitoring/](https://www.dotcom-monitor.com/server-monitor/dns-monitoring/)  and got a `Server Error in '/' Application.` at `https://userauth.dotcom-monitor.com/UserAuth/Account/FreeTrialSignUp`. Hm......

<amp-img src="/images/2015-08-08/error.png" alt="Ping Test" width="870px" height="149px" sizes="(min-width:800px) 60vw, 90vw"/>

# End of the day

I will look at other portions of the site as I have time.
The monitoring services could be very intersting.
So far the primary advantage [www.dotcom-tools.com](https://www.dotcom-tools.com) seems to be the global testing from diverse locations.
UI could use some work.
