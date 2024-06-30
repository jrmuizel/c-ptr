use c_ptr::{Ptr, TypeDesc, TypeInfo, malloc, free};


#[test]
fn structural_type_punning() {
    #[derive(TypeDesc, Default)]
    struct Point1 {
        x: f32,
        y: f32
    }

    #[derive(TypeDesc, Default)]
    struct Point2 {
        x: f32,
        y: f32
    }

    let r: Ptr<Point1> = malloc(std::mem::size_of::<Point1>()).cast();
    // this fails because we don't find the exact type in the metadata
    // instead we need to try to match up the fields individually
    //let k: Ptr<Point2> = r.clone().cast();
    free(r);
}

#[test]
fn array_type_punning() {
    #[derive(TypeDesc, Default)]
    struct Point {
        x: f32,
        y: f32
    }

    let r: Ptr<Point> = malloc(std::mem::size_of::<Point>()).cast();
    // this fails because we don't find the exact type in the metadata
    // instead we need to try to match up the fields individually
    // let k: Ptr<[f32; 2]> = r.clone().cast();
    free(r);
}