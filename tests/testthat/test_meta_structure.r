for (meta_name in all_metas) {
    e <- new.env()
    data(list = meta_name, package = "usdeaths", envir = e)
    meta <- get(meta_name, envir = e)

    test_that(paste("test_meta_structure", meta_name), {
        expect_true(all(c("name", "start", "end", "size", "type", "description", "codes") %in% names(meta)))
        expect_false(anyNA(meta$name))
        expect_false(anyNA(meta$start))
        expect_false(anyNA(meta$end))
        expect_false(anyNA(meta$size))
        expect_false(anyNA(meta$type))
    })

    test_that(paste("test_widths", meta_name), {
        expect_true(all(meta$end - meta$start + 1 == meta$size))
    })

    test_that(paste("test_positions", meta_name), {
        meta_sorted <- meta |> arrange(start)
        expect_true(all(meta_sorted$start[-1] == meta_sorted$end[-nrow(meta_sorted)] + 1))
    })

    test_that(paste("test_unique_names", meta_name), {
        expect_true(all(!duplicated(meta$name)))
    })

    test_that(paste("test_codes_parseable", meta_name), {
        coded <- meta |> filter(codes != "")
        pairs <- coded$codes |>
            str_split("\\|") |>
            unlist()
        expect_true(all(str_count(pairs, "=") == 1))
    })
}
