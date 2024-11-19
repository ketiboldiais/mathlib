import { getPostsSitemap } from "@/utils/mdxUtils";
import { MetadataRoute } from "next";


const BASE_URL = "https://example.com";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
    const posts = await getPostsSitemap();
    return [
        {url: BASE_URL},
        {url: BASE_URL + "/blog"},
        ...posts.map((post) => ({
            url: `${BASE_URL}/blog/${post.slug}`,
            lastModified: post.mtime
        }))
    ]
}