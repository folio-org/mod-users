CREATE OR REPLACE FUNCTION ${myuniversity}_${mymodule}.process_department_assign() RETURNS TRIGGER
AS $process_department_assign$
  DECLARE
  	dep_id text;
  BEGIN
    IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
      FOR dep_id IN (SELECT jsonb_array_elements_text(NEW.jsonb->'departments')) LOOP
        IF ((SELECT COUNT(*) FROM ${myuniversity}_${mymodule}.departments WHERE id::text = dep_id) != 1) THEN
          RAISE foreign_key_violation USING DETAIL = format('Key (departments)=(%s) is not present in table "departments".', dep_id);
        END IF;
      END LOOP;
    END IF;
    RETURN NEW;
  END;
$process_department_assign$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS process_department_assign_trigger ON ${myuniversity}_${mymodule}.users CASCADE;

CREATE TRIGGER process_department_assign_trigger
BEFORE INSERT OR UPDATE ON ${myuniversity}_${mymodule}.users FOR EACH ROW EXECUTE PROCEDURE ${myuniversity}_${mymodule}.process_department_assign();

CREATE OR REPLACE FUNCTION ${myuniversity}_${mymodule}.process_department_delete() RETURNS TRIGGER
AS $process_department_delete$
  DECLARE
    fk_counter integer := 0;
  BEGIN
    IF (TG_OP = 'DELETE') THEN
      SELECT COUNT(*) INTO fk_counter FROM ${myuniversity}_${mymodule}.users WHERE id = (SELECT id FROM ${myuniversity}_${mymodule}.users WHERE OLD.id::text = ANY (SELECT jsonb_array_elements_text(jsonb->'departments')) LIMIT 1);
        IF (fk_counter > 0) THEN
          RAISE foreign_key_violation USING DETAIL = format('Key (id)=(%s) is still referenced from table "users".', OLD.jsonb->>'id');
        END IF;
    END IF;
    RETURN OLD;
  END;
$process_department_delete$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS process_department_delete_trigger ON ${myuniversity}_${mymodule}.departments CASCADE;

CREATE TRIGGER process_department_delete_trigger
BEFORE DELETE ON ${myuniversity}_${mymodule}.departments FOR EACH ROW EXECUTE PROCEDURE ${myuniversity}_${mymodule}.process_department_delete();
