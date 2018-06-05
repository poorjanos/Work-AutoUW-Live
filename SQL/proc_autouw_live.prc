CREATE OR REPLACE PROCEDURE POORJ.kpm_LIVE
IS
BEGIN
   EXECUTE IMMEDIATE 'TRUNCATE TABLE t_kpm_LIVE_siker';

   EXECUTE IMMEDIATE 'TRUNCATE TABLE t_erk_kpm_LIVE';

   COMMIT;

   INSERT INTO t_kpm_LIVE_siker (F_VONALKOD)
      SELECT   DISTINCT f_vonalkod
        FROM   ab_t_akr_esemeny
       WHERE   f_datum >= TRUNC (SYSDATE, 'mm')
               AND f_esemeny IN
                        ('D2', 'M3', '7S', '7T', '72', '77', '7E', '7A', 'M3');

   INSERT INTO t_erk_kpm_LIVE (vonalkod)
      SELECT   DISTINCT f_vonalkod
        FROM   ab_t_akr_esemeny
       WHERE   f_datum BETWEEN TRUNC (SYSDATE, 'mm')
                           AND  SYSDATE
               AND f_esemeny IN
                        ('01',
                         '31',
                         '35',
                         '70',
                         '37',
                         '38',
                         'DE',
                         'DA',
                         'M0',
                         'M1',
                         'M2',
                         'M3',
                         'M4',
                         '7D',
                         'E0',
                         'E1',
                         'E3',
                         '28');

   COMMIT;


   UPDATE   t_erk_kpm_LIVE a
      SET   szerzazon =
               (  SELECT   MAX (f.f_szerz_azon)
                    FROM   ab_t_akr_ajanlat f
                   WHERE   f.f_vonalkod = a.vonalkod
                GROUP BY   f.f_vonalkod);

   COMMIT;


   UPDATE   t_erk_kpm_LIVE a
      SET   idoszak =
               (SELECT   TRUNC (SYSDATE, 'mm') FROM DUAL);

   COMMIT;



   UPDATE   t_erk_kpm_LIVE a
      SET   (modkod) =
               (SELECT   b.f_modkod
                  FROM   ab_t_akr_kotveny b
                 WHERE   b.f_szerz_azon = a.szerzazon);

   COMMIT;



   UPDATE   t_erk_kpm_LIVE a
      SET   erkdat =
               (SELECT   MIN (b.f_datum)
                  FROM   ab_t_akr_esemeny b
                 WHERE   b.f_esemeny IN
                               ('01',
                                '31',
                                '35',
                                '70',
                                '37',
                                '38',
                                'DE',
                                'DA',
                                'M0',
                                'M1',
                                'M2',
                                'M3',
                                'M4',
                                '7D',
                                'E0',
                                'E1',
                                'E3',
                                '28')
                         AND b.f_vonalkod = a.vonalkod);

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   (szerzdat) =
               (SELECT   MIN (b.f_datum)
                  FROM   ab_t_akr_esemeny b
                 WHERE   b.f_esemeny IN
                               ('14',
                                '18',
                                '50',
                                '51',
                                '52',
                                '53',
                                '72',
                                '77',
                                '79',
                                '7T',
                                '7S',
                                '7E',
                                '7A',
                                'M3',
                                'F4')
                         AND b.f_vonalkod = a.vonalkod);

   COMMIT;



   UPDATE   t_erk_kpm_LIVE a
      SET   elutdat =
               (SELECT   MIN (f_datum)
                  FROM   ab_t_akr_esemeny
                 WHERE   f_esemeny IN ('15', '19', '41', '45', '48')
                         AND f_vonalkod = a.vonalkod);

   COMMIT;



   UPDATE   t_erk_kpm_LIVE a
      SET   stornodat =
               (SELECT   MIN (f_datum)
                  FROM   ab_t_akr_esemeny
                 WHERE   f_esemeny IN ('04', '40', '9A', '9B', '9C')
                         AND f_vonalkod = a.vonalkod);

   COMMIT;



   UPDATE   t_erk_kpm_LIVE a
      SET   modtyp = 'Life'
    WHERE   a.modkod LIKE '13%' OR a.modkod LIKE '11%' OR a.modkod LIKE '15%';

   UPDATE   t_erk_kpm_LIVE a
      SET   modtyp = 'Vagyon'
    WHERE   a.modkod LIKE '21%'
            OR    a.modkod LIKE '12%'
              AND a.modkod NOT LIKE '217%'
              AND a.modkod NOT LIKE '218%'
              AND a.modkod NOT LIKE '2186%';

   UPDATE   t_erk_kpm_LIVE a
      SET   modtyp = 'Casco'
    WHERE   a.modkod LIKE '218%' AND a.modkod NOT LIKE '2186%';

   UPDATE   t_erk_kpm_LIVE a
      SET   modtyp = 'GFB'
    WHERE   a.modkod LIKE '35%';

   UPDATE   t_erk_kpm_LIVE a
      SET   modtyp = 'VVR'
    WHERE      a.modkod LIKE '217%'
            OR a.modkod LIKE '2186%'
            OR a.modkod LIKE '22%'
            OR a.modkod LIKE '23%'
            OR a.modkod LIKE '24%'
            OR a.modkod LIKE '33%'
            OR a.modkod LIKE '34%'
            OR a.modkod LIKE '36%';

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   erk_esemeny =
               (SELECT   DISTINCT
                         FIRST_VALUE(f_esemeny)
                            OVER (PARTITION BY f_vonalkod
                                  ORDER BY f_datum DESC)
                  FROM   ab_t_akr_esemeny b
                 WHERE   a.vonalkod = b.f_vonalkod
                         AND f_esemeny IN
                                  ('01',
                                   '31',
                                   '32',
                                   '35',
                                   '70',
                                   '37',
                                   '38',
                                   'DE',
                                   'DA',
                                   'M0',
                                   'M1',
                                   'M4',
                                   '7D',
                                   'E0',
                                   'E1',
                                   'E3',
                                   '28'));

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'Papir'
    WHERE   erk_esemeny IN ('01', '31');


   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'FE'
    WHERE   erk_esemeny IN ('32', '35');

   --FE

   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'Ajpotlo'
    WHERE   erk_esemeny IN ('70');


   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'Elektra'
    WHERE   erk_esemeny IN ('DE');


   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'Elek'
    WHERE   erk_esemeny IN ('DA');


   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'MySigno'
    WHERE   erk_esemeny IN ('M0', 'M1', 'M4');


   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'Tavert'
    WHERE   erk_esemeny IN ('37', '38', '7D', '28');


   UPDATE   t_erk_kpm_LIVE a
      SET   papir_tipus = 'Enyil'
    WHERE   erk_esemeny IN ('E0', 'E1', 'E3');

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   kpm = 'Sikeres'
    WHERE   vonalkod IN (SELECT   f_vonalkod FROM t_kpm_LIVE_siker);

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   kpm = 'Sikertelen'
    WHERE   vonalkod IN
                  (SELECT   f_vonalkod
                     FROM   ab_t_akr_esemeny
                    WHERE   f_datum >= TRUNC (SYSDATE, 'mm')
                            AND f_esemeny IN ('D3', '76', 'M2'))
            AND a.kpm is null;

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   kpm = 'Nincs'
    WHERE   kpm IS NULL;

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   (kpm_hiba_dat) =
               (SELECT   MIN (b.f_datum)
                  FROM   ab_t_akr_esemeny b
                 WHERE   b.f_esemeny IN ('D3', '76', 'M2')
                         AND b.f_vonalkod = a.vonalkod);

   COMMIT;

   UPDATE   t_erk_kpm_LIVE a
      SET   gfb_kotes =
               (SELECT   b.f_kotes_ok
                  FROM   ab_t_akr_kotveny_gfb b
                 WHERE   a.szerzazon = b.f_szerz_azon);

   COMMIT;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Tul_valtas'
    WHERE   gfb_kotes = 1;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'DNF_ujra'
    WHERE   gfb_kotes = 2;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Evford_valt'
    WHERE   gfb_kotes = 3;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Kozos_meg'
    WHERE   gfb_kotes = 4;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Uztart_valt'
    WHERE   gfb_kotes = 5;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Orokles'
    WHERE   gfb_kotes = 6;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Uj_fhely'
    WHERE   gfb_kotes = 7;

   UPDATE   t_erk_kpm_LIVE
      SET   gfb_kotes_nev = 'Egyeb'
    WHERE   gfb_kotes IS NOT NULL AND gfb_kotes > 7;

   COMMIT;

   DELETE FROM   t_erk_kpm_LIVE
         WHERE   erkdat < TRUNC(SYSDATE, 'mm');

   COMMIT;


   EXECUTE IMMEDIATE 'CREATE OR REPLACE SYNONYM fpack FOR pack@dl_exdb_pre2';

   EXECUTE IMMEDIATE 'CREATE OR REPLACE SYNONYM fproposal FOR proposal@dl_exdb_pre2';

   COMMIT;


   INSERT INTO t_erk_kpm_LIVE (idoszak,
                          vonalkod,
                          szerzazon,
                          modkod,
                          modtyp,
                          erkdat,
                          szerzdat,
                          elutdat,
                          stornodat,
                          papir_tipus,
                          erk_esemeny,
                          kpm,
                          kpm_hiba_dat,
                          gfb_kotes,
                          gfb_kotes_nev)
      SELECT   ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1),
               TO_CHAR (fproposal.proposal_idntfr),
               TO_CHAR (fpack.oid_to_idntfr (fproposal.contract_oid)),
               fproposal.product_code,
               'Life',
               fproposal.arrival_date,
               fpack.proposal_contract_date (fproposal.proposal_idntfr),
               fproposal.rejection_date,
               fpack.proposal_cancel_date (fproposal.proposal_idntfr),
               CASE
                  WHEN fproposal.front_end = 'N'
                  THEN
                     'Papir'
                  WHEN fproposal.front_end = 'I'
                       AND fproposal.front_end_type IS NULL
                  THEN
                     'FE'
                  WHEN fproposal.front_end_type = 'MYSIG'
                  THEN
                     'MySigno'
                  WHEN fproposal.front_end_type = 'ENYIL'
                  THEN
                     'Enyil'
                  WHEN fproposal.front_end_type = 'TAVERT'
                  THEN
                     'Tavert'
               END,
               NULL,
               'Nincs',
               NULL,
               NULL,
               NULL
        FROM   fproposal
       WHERE   fproposal.arrival_date BETWEEN TRUNC (SYSDATE, 'mm') AND  SYSDATE
               AND fproposal.cntry_flg LIKE 'HU';

   COMMIT;

   
   
EXECUTE IMMEDIATE 'TRUNCATE TABLE  t_autouw_dict';

   INSERT INTO t_autouw_dict (hibaazon, hiba)
      SELECT   DISTINCT
               FIRST_VALUE(hibaazon)
                  OVER (PARTITION BY hibaazon
                        ORDER BY gyak DESC
                        ROWS UNBOUNDED PRECEDING)
                  AS hibaazon,
               FIRST_VALUE(hiba)
                  OVER (PARTITION BY hibaazon
                        ORDER BY gyak DESC
                        ROWS UNBOUNDED PRECEDING)
                  AS hiba
        FROM   (  SELECT   DISTINCT hibaazon, hiba, COUNT (szerzazon) AS gyak
                    FROM   (SELECT   a.szerzazon,
                                     modtyp,
                                     papir_tipus,
                                     gfb_kotes_nev,
                                     TRUNC (a.erkdat, 'mm') AS idoszak,
                                     f_hibaszam AS hibaazon,
                                     CASE
                                        WHEN INSTR (
                                                REGEXP_REPLACE (
                                                   f_szoveg,
                                                   '[[:digit:]]|\([^()]*\)',
                                                   ''
                                                ),
                                                '.'
                                             ) <> 0
                                        THEN
                                           SUBSTR (
                                              REGEXP_REPLACE (
                                                 f_szoveg,
                                                 '[[:digit:]]|\([^()]*\)',
                                                 ''
                                              ),
                                              0,
                                              INSTR (
                                                 REGEXP_REPLACE (
                                                    f_szoveg,
                                                    '[[:digit:]]|\([^()]*\)',
                                                    ''
                                                 ),
                                                 '.'
                                              )
                                           )
                                        WHEN INSTR (
                                                REGEXP_REPLACE (
                                                   f_szoveg,
                                                   '[[:digit:]]|\([^()]*\)',
                                                   ''
                                                ),
                                                ':'
                                             ) <> 0
                                        THEN
                                           SUBSTR (
                                              REGEXP_REPLACE (
                                                 f_szoveg,
                                                 '[[:digit:]]|\([^()]*\)',
                                                 ''
                                              ),
                                              0,
                                              INSTR (
                                                 REGEXP_REPLACE (
                                                    f_szoveg,
                                                    '[[:digit:]]|\([^()]*\)',
                                                    ''
                                                 ),
                                                 ':'
                                              )
                                           )
                                        ELSE
                                           REGEXP_REPLACE (
                                              f_szoveg,
                                              '[[:digit:]]|\([^()]*\)',
                                              ''
                                           )
                                     END
                                        AS hiba
                              FROM   t_erk_kpm_LIVE a, ab_t_akr_naplo b
                             WHERE       a.szerzazon = b.f_szerz_azon
                                     AND a.kpm_hiba_dat = b.f_datum
                                     AND a.kpm = 'Sikertelen')
                GROUP BY   hibaazon, hiba
                ORDER BY   hibaazon, hiba);
COMMIT;   
   

EXECUTE IMMEDIATE 'TRUNCATE TABLE t_kpm_LIVE_pattern';
COMMIT;

INSERT INTO t_kpm_LIVE_pattern (vonalkod,
                                            szerzazon,
                                            idoszak,
                                            modtyp,
                                            modkod,
                                            papir_tipus,
                                            gfb_kotes_nev,
                                            hiba_minta)
      SELECT   vonalkod,
               szerzazon,
               idoszak,
               modtyp,
               modkod,
               papir_tipus,
               gfb_kotes_nev,
               SUBSTR (trace, 0, INSTR (trace, '***0') - 1) AS hiba_minta
        FROM   (SELECT   vonalkod,
                         szerzazon,
                         idoszak,
                         modtyp,
                         modkod,
                         papir_tipus,
                         gfb_kotes_nev,
                            e1
                         || '***'
                         || e2
                         || '***'
                         || e3
                         || '***'
                         || e4
                         || '***'
                         || e5
                         || '***'
                         || e6
                         || '***'
                         || e7
                         || '***'
                         || e8
                         || '***'
                         || e9
                         || '***'
                         || e10
                         || '***'
                         || e11
                         || '***'
                         || e12
                            AS trace
                  FROM   (SELECT   DISTINCT
                                   vonalkod,
                                   szerzazon,
                                   idoszak,
                                   modtyp,
                                   modkod,
                                   papir_tipus,
                                   gfb_kotes_nev,
                                   FIRST_VALUE(e1)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e1,
                                   FIRST_VALUE(e2)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e2,
                                   FIRST_VALUE(e3)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e3,
                                   FIRST_VALUE(e4)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e4,
                                   FIRST_VALUE(e5)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e5,
                                   FIRST_VALUE(e6)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e6,
                                   FIRST_VALUE(e7)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e7,
                                   FIRST_VALUE(e8)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e8,
                                   FIRST_VALUE(e9)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e9,
                                   FIRST_VALUE(e10)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e10,
                                   FIRST_VALUE(e11)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e11,
                                   FIRST_VALUE(e12)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e12,
                                   FIRST_VALUE(e13)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e13,
                                   FIRST_VALUE(e14)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e14
                            FROM   (SELECT   vonalkod,
                                             szerzazon,
                                             idoszak,
                                             modtyp,
                                             modkod,
                                             papir_tipus,
                                             gfb_kotes_nev,
                                             hiba AS e1,
                                             LEAD (
                                                hiba,
                                                1,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e2,
                                             LEAD (
                                                hiba,
                                                2,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e3,
                                             LEAD (
                                                hiba,
                                                3,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e4,
                                             LEAD (
                                                hiba,
                                                4,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e5,
                                             LEAD (
                                                hiba,
                                                5,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e6,
                                             LEAD (
                                                hiba,
                                                6,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e7,
                                             LEAD (
                                                hiba,
                                                8,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e8,
                                             LEAD (
                                                hiba,
                                                9,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e9,
                                             LEAD (
                                                hiba,
                                                10,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e10,
                                             LEAD (
                                                hiba,
                                                12,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e11,
                                             LEAD (
                                                hiba,
                                                13,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e12,
                                             LEAD (
                                                hiba,
                                                14,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e13,
                                             LEAD (
                                                hiba,
                                                15,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e14,
                                             hibaazon
                                      FROM   (SELECT   c.*, d.hiba
                                                FROM   (  SELECT   DISTINCT
                                                                   vonalkod,
                                                                   a.szerzazon,
                                                                   modtyp,
                                                                   modkod,
                                                                   papir_tipus,
                                                                   gfb_kotes_nev,
                                                                   TRUNC (
                                                                      a.erkdat,
                                                                      'mm'
                                                                   )
                                                                      AS idoszak,
                                                                   f_hibaszam
                                                                      AS hibaazon
                                                            FROM   t_erk_kpm_LIVE a,
                                                                   ab_t_akr_naplo b
                                                           WHERE   a.szerzazon =
                                                                      b.f_szerz_azon
                                                                   AND a.kpm_hiba_dat =
                                                                         b.f_datum
                                                                   AND a.kpm =
                                                                         'Sikertelen'
                                                                   AND f_hibaszam <>
                                                                         '99999'
                                                        ORDER BY   idoszak,
                                                                   a.szerzazon,
                                                                   f_hibaszam)
                                                       c,
                                                       t_autouw_dict d
                                               WHERE   c.hibaazon =
                                                          d.hibaazon))));
COMMIT;
   
END kpm_LIVE;
/