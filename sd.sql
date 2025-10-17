SELECT * FROM chuongtrinhdaotao;
UPDATE sinhvien
SET hoDem = "Dinh", ten ="huy"
WHERE (hoDem IS NULL) AND (ten IS NULL);
SELECT*FROM sinhvien;