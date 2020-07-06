CREATE OR REPLACE FUNCTION process_department_assign() RETURNS TRIGGER
AS $process_department_assign$
  DECLARE
  	dep_id text;
    fk_counter integer := 0;
  BEGIN
    IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
      FOR dep_id IN (SELECT jsonb_array_elements_text(NEW.jsonb->'departments')) LOOP
        IF ((SELECT COUNT(*) FROM departments WHERE id::text = dep_id) != 1) THEN
          RAISE foreign_key_violation USING DETAIL = format('Key (departments)=(%s) is not present in table "departments".', dep_id);
        END IF;
        fk_counter := 0;
      END LOOP;
    END IF;
    RETURN NEW;
  END;
$process_department_assign$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS process_department_assign_trigger ON users CASCADE;

CREATE TRIGGER process_department_assign_trigger
BEFORE INSERT OR UPDATE ON users FOR EACH ROW EXECUTE PROCEDURE process_department_assign();

CREATE OR REPLACE FUNCTION process_department_delete() RETURNS TRIGGER
AS $process_department_delete$
  DECLARE
    fk_counter integer := 0;
  BEGIN
    IF (TG_OP = 'DELETE') THEN
      SELECT COUNT(*) INTO fk_counter FROM users WHERE OLD.jsonb->>'id' = ANY (SELECT jsonb_array_elements_text(jsonb->'departments'));
        IF (fk_counter > 0) THEN
          RAISE foreign_key_violation USING DETAIL = format('Key (id)=(%s) is still referenced from table "users".', OLD.jsonb->>'id');
        END IF;
    END IF;
    RETURN OLD;
  END;
$process_department_delete$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS process_department_delete_trigger ON departments CASCADE;

CREATE TRIGGER process_department_delete_trigger
BEFORE DELETE ON departments FOR EACH ROW EXECUTE PROCEDURE process_department_delete();
