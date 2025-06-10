library(sf)
library(tidyverse)
in_kml <- st_read(r"(C:\Users\aberd\Downloads\Web_Data_2023\doc.kml)") |>
  st_zm()
useless_html = c(r"(<html xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:msxsl="urn:schemas-microsoft-com:xslt"> <head> <META http-equiv="Content-Type" content="text/html"> <meta http-equiv="content-type" content="text/html; charset=UTF-8"> </head> <body style="margin:0px 0px 0px 0px;overflow:auto;background:#FFFFFF;"> <table style="font-family:Arial,Verdana,Times;font-size:12px;text-align:left;width:100%;border-collapse:collapse;padding:3px 3px 3px 3px"> <tr style="text-align:center;font-weight:bold;background:#9CBCE2"> )",
                 r"(</tr> <tr> <td> <table style="font-family:Arial,Verdana,Times;font-size:12px;text-align:left;width:100%;border-spacing:0px; padding:3px 3px 3px 3px"> )",
                 "<td>",
                 "</td>",
                 r"(<tr bgcolor="#D4E4F3"> )",
                 r"(</tr>)",
                 "<tr>",
                 "</table>",
                 "</table>", "</body>", "</html>")

phil_df <- in_kml |> mutate(tst = str_remove_all(Description, str_flatten(useless_html, "|"))) |> select(-Description) |>
  separate_wider_delim(tst, "  ", names_sep = "_") |> select(c(Name, tst_2, tst_4, geometry)) |> 
  rename(FID = tst_2,
         Type = tst_4) |>
  mutate(across(2:3, ~ trimws(.) |> str_remove_all("FID |Type "))) |>
  st_as_sf() |>
  mutate(
    x_coord = round(st_coordinates(geometry)[, 1], 7),
    y_coord = round(st_coordinates(geometry)[, 2], 7)
  ) |>
  st_drop_geometry() |>
  select(-c(FID)) |>
  mutate(Object = paste0("Object: ", y_coord, ", ", x_coord),
    Icon = case_when(Type == "Trailhead" ~ paste0(r"(Icon: 0,0,000,0,1,"Trailhead: )", Name, r"(")"),
                     Type == "Peak" ~ paste0(r"(Icon: 0,0,000,0,2,"Peak: )", Name, r"(")"),
                     Type == "Program" ~ paste0(r"(Icon: 0,0,000,0,3,"Program: )", Name, r"(")")),
    out = paste0(Object, "\n", Icon, "\n", "End:"))


header <- c(r"(Refresh: 999
Threshold: 999
Title: Philmont Scout Ranch -- Landmarks
IconFile: 0,22,22,11,11,"https://raw.githubusercontent.com/oonuttlet/philmont-placefile/0c7b95874750572aa5ffbf05e1b0ce37bd5b2100/bin/philmont_features.png")", "")

writeLines(str_flatten(c(header, phil_df$out), "\n"), r"(D:\repos\philmont-placefile\philmont-places.txt)")


