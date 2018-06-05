  SELECT   DISTINCT a.szerzazon,
                    modtyp,
                    modkod,
                    papir_tipus,
                    gfb_kotes_nev,
                    TRUNC (a.erkdat, 'mm') AS idoszak,
                    f_hibaszam AS hibaazon
    FROM    t_erk_kpm_live a, ab_t_akr_naplo b
   WHERE       a.szerzazon = b.f_szerz_azon
           AND a.kpm_hiba_dat = b.f_datum
           AND a.kpm = 'Sikertelen'
           AND f_hibaszam <> '99999'
ORDER BY   idoszak, a.szerzazon, f_hibaszam