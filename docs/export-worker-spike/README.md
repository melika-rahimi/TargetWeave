# Background export worker spike

Paused: a second R process plus Shiny does not fit Render Free (512 MB), and `ggsave` PNG→base64 on the main process was what tripped the 5-second health check.

This release keeps HTML/ZIP on the Shiny process and uses inline HTML/CSS/SVG instead of raster plots. Do not revive a job queue or worker on 512 MB.
